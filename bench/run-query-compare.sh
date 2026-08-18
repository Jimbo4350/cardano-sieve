#!/usr/bin/env bash
#
# Query-latency head-to-head: cardano-sieve vs kupo, over HTTP.
#
# This script MEASURES; it does not prepare. On every run it first reports a
# readiness checklist — sieve db, sieve indexes, kupo db, both servers — and if
# anything is missing it prints the numbered commands for exactly those steps and
# exits 0. Nothing long-running happens behind your back: a not-ready run costs
# seconds, and the multi-minute sync is a command you run in a terminal you are
# watching (it prints a progress heartbeat).
#
# Once everything is ready, it runs each query shape on BOTH servers (kupo and
# sieve share the pattern grammar, so the URL is identical) and reports the two
# side by side. The work is made EQUAL first: kupo streams every match in one
# response, sieve serves 100-row pages behind an X-Next-Cursor header, so the
# sieve side of every case walks the full cursor chain — every page, every row —
# and its time is the sum of the per-request times. The row totals of the two
# walks are compared and any mismatch is flagged, so a number is never quietly
# measuring different work.
#
# Sampling is INTERLEAVED (sieve, kupo, sieve, kupo …) and judged on the PAIRED
# per-round deltas, the run-flush-check.sh method: this box is shared with a
# desktop and a cardano-node, so absolute times wander; the difference within a
# back-to-back pair mostly cancels that, and the sign-consistency of the deltas
# says whether the answer is trustworthy.
#
# The two servers are long-lived and live in their own terminals; this script
# never starts or stops them, it just measures against them.
#
# PREP=1 opts into the old behaviour — sync + build-indexes inline, unattended —
# for CI or a scripted re-run.
#
# Env (defaults shown):
#   UNTIL_SLOT=4000000  NODE_SOCKET=$HOME/node.socket  TESTNET_MAGIC=2
#   NODE_CONFIG=$HOME/cardano-bin/11.0.1/share/preview/config.json
#   SIEVE_PORT=3030  KUPO_PORT=1442  N=200  WARMUP=10
#   SIEVE_BIN / KUPO_BIN (else discovered via cabal)
#   PREP=1 to prepare the sieve db inline instead of printing instructions
#
# CAVEAT (index parity): a fair race needs BOTH tools' query indexes built.
# sieve's come from --build-indexes (step 2 of the checklist); kupo builds its
# non-essential indexes only on reaching the node's real tip, which a bounded
# --until sync never does — so the script checks kupo's DB for the address index
# and WARNS if it is absent.
#
# CAVEAT (coverage): this measures ONE dimension, unspent-by-address, because that
# is all Cardano.Sieve.Server.Run implements. The policy/asset query shapes live only in
# bench/run-query-bench.sh.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
UNTIL_SLOT="${UNTIL_SLOT:-4000000}" # reuses the run-query-bench sieve cache (no re-sync)
NODE_SOCKET="${NODE_SOCKET:-$HOME/node.socket}"
TESTNET_MAGIC="${TESTNET_MAGIC:-2}"
NODE_CONFIG="${NODE_CONFIG:-$HOME/cardano-bin/11.0.1/share/preview/config.json}"
SIEVE_PORT="${SIEVE_PORT:-3030}" # 3000 is cardano-node's P2P port on this box
KUPO_PORT="${KUPO_PORT:-1442}"
N="${N:-200}"
WARMUP="${WARMUP:-10}"
CACHE="${CACHE:-${XDG_CACHE_HOME:-$HOME/.cache}}"
SIEVE_DB="${SIEVE_DB:-$CACHE/cardano-sieve-bench/preview-until-$UNTIL_SLOT.sqlite}"
KUPO_DIR="${KUPO_DIR:-$CACHE/cardano-kupo-bench/preview-until-$UNTIL_SLOT}"
SIEVE_BIN="${SIEVE_BIN:-$(cd "$REPO_ROOT" && cabal list-bin cardano-sieve 2>/dev/null || true)}"
KUPO_BIN="${KUPO_BIN:-$(cd "$HOME/repos/kupo" && cabal list-bin kupo 2>/dev/null || true)}"
SIEVE_URL="http://127.0.0.1:$SIEVE_PORT"
KUPO_URL="http://127.0.0.1:$KUPO_PORT"

log() { printf '\033[1;36m[qcmp]\033[0m %s\n' "$*"; }
die() { echo "error: $*" >&2; exit 1; }

for t in curl jq sqlite3; do command -v "$t" >/dev/null || die "need $t on PATH"; done

# --- inspect the sieve database (read-only; does NOT create or sync anything) --
# [ -s ] first, because sqlite3 creates an empty file just by opening a missing
# path — which would then look like a synced-but-empty db.
sieve_db_rows() {
  [ -s "$SIEVE_DB" ] || { echo 0; return; }
  sqlite3 -readonly "$SIEVE_DB" "SELECT count(*) FROM unspent" 2>/dev/null || echo 0
}
sieve_db_indexed() {
  [ -s "$SIEVE_DB" ] || return 1
  sqlite3 -readonly "$SIEVE_DB" \
    "SELECT 1 FROM sqlite_master WHERE type='index' AND name='unspentByAddress'" 2>/dev/null |
    grep -q .
}

# Optional escape hatch for unattended use (CI, a scripted re-run): PREP=1 does
# the sync and index build inline instead of printing instructions. Off by
# default — a 4M sync is minutes long and belongs in a terminal someone is
# watching, not silently inside a measurement script.
prep_sieve_db() {
  [ -x "${SIEVE_BIN:-}" ] || die "cardano-sieve binary not found (build it, or set SIEVE_BIN)"
  if [ "$(sieve_db_rows)" -eq 0 ]; then
    log "PREP=1: syncing sieve db origin..$UNTIL_SLOT ..."
    [ -S "$NODE_SOCKET" ] || die "no node socket at $NODE_SOCKET"
    mkdir -p "$(dirname "$SIEVE_DB")"
    local tmp="$SIEVE_DB.partial"; rm -f "$tmp" "$tmp"-wal "$tmp"-shm
    "$SIEVE_BIN" --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" \
      --database "$tmp" --since origin --until "$UNTIL_SLOT" \
      || { rm -f "$tmp" "$tmp"-wal "$tmp"-shm; die "sieve sync failed — is cardano-node running and serving $NODE_SOCKET?"; }
    mv "$tmp" "$SIEVE_DB"; [ -f "$tmp-wal" ] && mv "$tmp-wal" "$SIEVE_DB-wal" || true
  fi
  sieve_db_indexed || {
    log "PREP=1: building sieve query indexes ..."
    "$SIEVE_BIN" \
      --database "$SIEVE_DB" --build-indexes >/dev/null
  }
}

# --- is each server up? -----------------------------------------------------
# Probe with a VALID pattern that matches nothing: an output reference whose
# transaction id is all zeroes. "00" used to serve as the probe, but an
# unparseable pattern is now a 400 rather than an empty list, so -sf treated a
# live server as down. A wildcard would work but streams the entire live set.
SIEVE_PROBE="/matches/0@$(printf '0%.0s' $(seq 64))"
sieve_up() { curl -sf -o /dev/null --max-time 5 "$SIEVE_URL$SIEVE_PROBE" 2>/dev/null; }
# kupo's /health is Prometheus text unless you ask for JSON.
kupo_slot() { curl -sf -H 'Accept: application/json' --max-time 3 "$KUPO_URL/health" 2>/dev/null | jq -r '.most_recent_checkpoint // 0' 2>/dev/null || echo 0; }
# Ready = kupo has reached the slot sieve reached. Both stop at the last block
# <= UNTIL_SLOT, whose slot is a bit below UNTIL_SLOT, so don't compare to UNTIL_SLOT.
kupo_up() { local s; s=$(kupo_slot); [ "${s:-0}" -ge "${REACHED:-$UNTIL_SLOT}" ] 2>/dev/null; }

# --- report what is missing and print only the commands for those ------------
# Ordered so the operator can work top to bottom: the db must exist before the
# sieve server can serve from it, and kupo must reach the target slot before it
# answers. Steps that are already satisfied are omitted rather than printed as
# no-ops, so what is on screen is exactly what is left to do.
print_setup() { # $1..$n = the missing step keys
  local step=0 missing=" $* "
  mkdir -p "$KUPO_DIR"
  printf '\nNot ready. Do the steps below, then re-run this script.\n'

  case "$missing" in *" sieve-db "*)
    step=$((step + 1))
    cat <<EOF

  ── step $step: sync the sieve database (minutes; prints a progress heartbeat) ──
  '$SIEVE_BIN' --socket-path '$NODE_SOCKET' --testnet-magic $TESTNET_MAGIC \\
    --database '$SIEVE_DB' --since origin --until $UNTIL_SLOT
EOF
  esac

  case "$missing" in *" sieve-indexes "*)
    step=$((step + 1))
    cat <<EOF

  ── step $step: build the sieve query indexes (seconds) ──
  '$SIEVE_BIN' \\
    --database '$SIEVE_DB' --build-indexes
EOF
  esac

  case "$missing" in *" kupo-server "*)
    step=$((step + 1))
    cat <<EOF

  ── step $step: kupo, in its OWN terminal (leave it running) ──
  '$KUPO_BIN' --node-socket '$NODE_SOCKET' --node-config '$NODE_CONFIG' \\
    --since origin --until $UNTIL_SLOT --match '*' \\
    --workdir '$KUPO_DIR' --host 127.0.0.1 --port $KUPO_PORT

  Wait for it to reach the target slot:
    curl -s -H 'Accept: application/json' $KUPO_URL/health | jq .most_recent_checkpoint
EOF
  esac

  case "$missing" in *" sieve-server "*)
    step=$((step + 1))
    cat <<EOF

  ── step $step: the sieve query server, in its OWN terminal (leave it running) ──
  '$SIEVE_BIN' \\
    --database '$SIEVE_DB' --serve $SIEVE_PORT
EOF
  esac

  printf '\n(PREP=1 %s does steps 1-2 for you, unattended.)\n' "$0"
}

# --- measurement ------------------------------------------------------------
# Per round we keep two figures for each tool:
#   ttfb  time to first byte of the FIRST response: how fast an answer starts
#   total the whole answer: kupo's single response; sieve's page walk, summed
# Summing curl's per-page time_total (rather than wall-clocking the walk) keeps
# bash loop overhead out of sieve's number; every page still pays its real HTTP
# connect + request cost, which is the honest price of the paging design.
HDR="$(mktemp)"
trap 'rm -f "$HDR"' EXIT

# Walk one sieve query to exhaustion, following X-Next-Cursor with ?after=.
# Echoes "ttfb total pages" (seconds); fails if any page fails.
sieve_walk() { # $1 = path (already carries ?…, so the cursor appends with &)
  local after='' u t1 t2 ttfb='' total=0 pages=0 cur
  while :; do
    u="$SIEVE_URL$1"; [ -n "$after" ] && u="$u&after=$after"
    read -r t1 t2 < <(curl -s -D "$HDR" -o /dev/null --fail --max-time 600 \
      -w '%{time_starttransfer} %{time_total}\n' "$u" 2>/dev/null) || return 1
    [ -z "$ttfb" ] && ttfb="$t1"
    total=$(awk -v a="$total" -v b="$t2" 'BEGIN{printf "%.6f", a + b}')
    pages=$((pages + 1))
    cur=$(awk 'tolower($1) == "x-next-cursor:" { gsub(/\r/, "", $2); print $2 }' "$HDR")
    [ -z "$cur" ] && break
    after="$cur"
  done
  echo "$ttfb $total $pages"
}

# The same walk, counting rows instead of timing — run once per case to verify
# both tools hand back the SAME number of rows before any time is compared.
sieve_rows() { # $1 = path; echoes the row total across all pages
  local after='' u n total=0 cur
  while :; do
    u="$SIEVE_URL$1"; [ -n "$after" ] && u="$u&after=$after"
    n=$(curl -s -D "$HDR" --fail --max-time 600 "$u" 2>/dev/null | jq 'length') || return 1
    total=$((total + n))
    cur=$(awk 'tolower($1) == "x-next-cursor:" { gsub(/\r/, "", $2); print $2 }' "$HDR")
    [ -z "$cur" ] && break
    after="$cur"
  done
  echo "$total"
}

# One kupo request. Echoes "ttfb total" (seconds).
kupo_one() { # $1 = path
  curl -s -o /dev/null --fail --max-time 600 \
    -w '%{time_starttransfer} %{time_total}\n' "$KUPO_URL$1" 2>/dev/null
}

median() { sort -n | awk '{a[NR]=$1} END{ if (NR == 0) { print 0; exit } print (NR % 2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2 }'; }
ms() { awk -v x="$1" 'BEGIN{printf "%.1f", x * 1000}'; }

# Race one query shape on both servers: verify equal rows, then n interleaved
# rounds of (sieve walk, kupo stream) back to back, judged on the paired deltas.
#
# Rounds scale DOWN with result size. A hot key means a multi-second walk against
# a multi-second stream; n=200 there would take an hour to say what three paired
# rounds already say. Small results keep the full N, where per-request noise is
# what actually needs averaging out. The chosen n is printed with each case so a
# number is never read as more precise than it is.
race() { # $1 = label  $2 = path  $3 = note
  local sc kc n wr i pages='?'
  printf '\n%s  (%s)\n' "$1" "$3"
  sc=$(sieve_rows "$2" || echo '?')
  kc=$(curl -sf --max-time 600 "$KUPO_URL$2" 2>/dev/null | jq 'length' 2>/dev/null || echo '?')
  case "$kc" in
    ''|'?') n=3 ;;
    *) if [ "$kc" -gt 20000 ]; then n=3; elif [ "$kc" -gt 2000 ]; then n=8; else n=$N; fi ;;
  esac
  wr=$(( n < N ? 1 : WARMUP ))
  printf '  rows: sieve=%s kupo=%s%s   [n=%s, warmup=%s]\n' "$sc" "$kc" \
    "$([ "$sc" = "$kc" ] && echo ' (equal work)' || echo '  <- DIFFER, times below are not comparable')" "$n" "$wr"
  for ((i = 0; i < wr; i++)); do sieve_walk "$2" >/dev/null || true; kupo_one "$2" >/dev/null || true; done
  local s_ttfb=() s_total=() k_ttfb=() k_total=() st sx sp kt kx
  for ((i = 0; i < n; i++)); do
    read -r st sx sp < <(sieve_walk "$2") || { log "  sieve walk failed on round $i"; continue; }
    read -r kt kx < <(kupo_one "$2")      || { log "  kupo request failed on round $i"; continue; }
    s_ttfb+=("$st"); s_total+=("$sx"); k_ttfb+=("$kt"); k_total+=("$kx"); pages="$sp"
  done
  [ "${#s_total[@]}" -gt 0 ] || { printf '  no successful paired rounds\n'; return 0; }
  printf '  %-5s ttfb p50 %8s ms   total p50 %10s ms  (%s request(s)/round)\n' \
    "sieve" "$(ms "$(printf '%s\n' "${s_ttfb[@]}" | median)")" \
    "$(ms "$(printf '%s\n' "${s_total[@]}" | median)")" "$pages"
  printf '  %-5s ttfb p50 %8s ms   total p50 %10s ms  (1 request/round)\n' \
    "kupo" "$(ms "$(printf '%s\n' "${k_ttfb[@]}" | median)")" \
    "$(ms "$(printf '%s\n' "${k_total[@]}" | median)")"
  # Judge the PAIRED total-time difference, not the absolutes (see header).
  local deltas=()
  for ((i = 0; i < ${#s_total[@]}; i++)); do
    deltas+=("$(awk -v a="${s_total[$i]}" -v b="${k_total[$i]}" 'BEGIN{printf "%.6f", a - b}')")
  done
  awk -v dm="$(printf '%s\n' "${deltas[@]}" | median)" \
      -v km="$(printf '%s\n' "${k_total[@]}" | median)" \
      -v ds="$(printf '%s ' "${deltas[@]}")" 'BEGIN{
    split(ds, d, " "); pos = 0; tot = 0;
    for (i in d) { if (d[i] != "") { tot++; if (d[i] > 0) pos++ } }
    p = (km > 0) ? 100 * dm / km : 0;
    printf "  paired delta (sieve - kupo)  median %+.1f ms  (%+.1f%% of kupo)  %d/%d rounds sieve slower\n", \
      dm * 1000, p, pos, tot;
  }'
}

# --- run --------------------------------------------------------------------
[ -n "${PREP:-}" ] && prep_sieve_db

# One readiness pass, reported as a checklist before anything is measured, so a
# not-ready run costs seconds instead of blocking on a sync.
MISSING=""
ROWS=$(sieve_db_rows)

if [ "$ROWS" -eq 0 ]; then
  log "sieve db:      MISSING or empty  ($SIEVE_DB)"
  MISSING="$MISSING sieve-db sieve-indexes"
  REACHED=$UNTIL_SLOT
else
  REACHED=$(sqlite3 -readonly "$SIEVE_DB" "SELECT max(created_slot) FROM unspent")
  log "sieve db:      ok  ($ROWS unspent rows, reached slot $REACHED)"
  if sieve_db_indexed; then
    log "sieve indexes: ok"
  else
    log "sieve indexes: MISSING  (queries would be unfairly slow)"
    MISSING="$MISSING sieve-indexes"
  fi
fi

if [ -s "$KUPO_DIR/kupo.sqlite3" ]; then
  log "kupo db:       ok  ($KUPO_DIR/kupo.sqlite3)"
else
  log "kupo db:       absent — kupo will sync it on first start (slow)"
fi

if sieve_up; then log "sieve server:  up  ($SIEVE_URL)"
else
  log "sieve server:  DOWN  ($SIEVE_URL)"
  MISSING="$MISSING sieve-server"
fi

if kupo_up; then log "kupo server:   up  ($KUPO_URL)"
else
  log "kupo server:   DOWN  ($KUPO_URL — need slot >= $REACHED, have $(kupo_slot))"
  MISSING="$MISSING kupo-server"
fi

if [ -n "$MISSING" ]; then
  print_setup $MISSING
  exit 0
fi
log "all ready — sieve=$SIEVE_URL  kupo=$KUPO_URL"

# Fairness check: does kupo have an address index?
KUPO_SQLITE="$KUPO_DIR/kupo.sqlite3"
if [ -f "$KUPO_SQLITE" ] && ! sqlite3 "$KUPO_SQLITE" "SELECT name FROM sqlite_master WHERE type='index'" 2>/dev/null | grep -qi address; then
  log "WARNING: kupo has no address index (bounded sync never hit real tip) — kupo numbers are unfairly slow."
fi

# Cases. Sieve caps results at pageLimit (100) while kupo streams every match, so
# only keys with <= 100 matches compare like with like — a hotter key would put
# sieve's 100 rows against all of kupo's and the times would measure different work.
# The race() helper reports "rows: sieve=N kupo=M <- DIFFER" when that happens, so a
# mismatch is visible rather than silently skewing a number.
#
# The hot-key cases below are deliberately kept and WILL report DIFFER while the cap
# is in place: their ttfb figures are still meaningful (sieve answers a hot key in
# ~1 ms against kupo's 170-660 ms) and they are the cases to watch when the cap is
# eventually lifted.
#
# Addresses are base16 (both servers accept it); policy and asset use the pattern
# grammar, which both also share.
# <= 100 so sieve's cap does not truncate: this is the one fully comparable case.
SMALL_ADDR=$(sqlite3 "$SIEVE_DB" "SELECT hex(address) FROM unspent GROUP BY address HAVING count(*) <= 100 ORDER BY count(*) DESC LIMIT 1")
BIG_ADDR=$(sqlite3 "$SIEVE_DB" "SELECT hex(address) FROM unspent GROUP BY address ORDER BY count(*) DESC LIMIT 1")
POL=$(sqlite3 "$SIEVE_DB" "SELECT lower(hex(d.policy_id)) FROM policies p JOIN policy_ids d USING(policy_num) GROUP BY p.policy_num ORDER BY count(*) DESC LIMIT 1")
read -r APOL ANAME <<<"$(sqlite3 "$SIEVE_DB" -separator ' ' "SELECT lower(hex(d.policy_id)), lower(hex(p.asset_name)) FROM policies p JOIN policy_ids d USING(policy_num) WHERE length(p.asset_name) > 0 GROUP BY p.policy_num, p.asset_name ORDER BY count(*) DESC LIMIT 1")"

n_of() { sqlite3 "$SIEVE_DB" "$1" 2>/dev/null || echo '?'; }
log "cases: N=$N per case, warmup=$WARMUP"

race "address, comparable" "/matches/$SMALL_ADDR?unspent" \
  "$(n_of "SELECT count(*)||' utxos' FROM unspent WHERE address = X'$SMALL_ADDR'")"
race "address, hottest (capped)" "/matches/$BIG_ADDR?unspent" \
  "$(n_of "SELECT count(*)||' utxos' FROM unspent WHERE address = X'$BIG_ADDR'")"
race "policy, hottest (capped)" "/matches/$POL.*?unspent" \
  "$(n_of "SELECT count(*)||' policy rows over full history' FROM policies WHERE policy_num=(SELECT policy_num FROM policy_ids WHERE policy_id=X'${POL^^}')")"
race "asset, hottest (capped)" "/matches/$APOL.$ANAME?unspent" "one specific asset"
race "wildcard, unspent (capped)" "/matches/*?unspent" \
  "$(n_of "SELECT count(*)||' rows' FROM unspent")"

# Dump one full row from each so the response SHAPES can be compared directly.
# Uses the small-address case: a hot key would write hundreds of megabytes to /tmp.
curl -s --max-time 60 "$SIEVE_URL/matches/$SMALL_ADDR?unspent" >/tmp/qcmp-sieve.json 2>/dev/null || true
curl -s --max-time 60 "$KUPO_URL/matches/$SMALL_ADDR?unspent"  >/tmp/qcmp-kupo.json  2>/dev/null || true
printf '\nsample match (first row) — full JSON in /tmp/qcmp-{sieve,kupo}.json:\n'
echo "  sieve:"; jq -S '.[0]' /tmp/qcmp-sieve.json 2>/dev/null | sed 's/^/    /'
echo "  kupo :"; jq -S '.[0]' /tmp/qcmp-kupo.json  2>/dev/null | sed 's/^/    /'
