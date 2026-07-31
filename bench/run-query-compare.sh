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
# Once everything is ready, it hits GET /matches/{addr}?unspent on BOTH servers
# (kupo and sieve both accept a base16 address, so the URL is identical) N times
# and reports p50/p95/p99 (ms) side by side.
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
# is all Cardano.Server.Http implements. The policy/asset query shapes live only in
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
    "$SIEVE_BIN" --socket-path /tmp/unused.sock --testnet-magic "$TESTNET_MAGIC" \
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
  '$SIEVE_BIN' --socket-path /tmp/unused.sock --testnet-magic $TESTNET_MAGIC \\
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
  '$SIEVE_BIN' --socket-path /tmp/unused.sock --testnet-magic $TESTNET_MAGIC \\
    --database '$SIEVE_DB' --serve $SIEVE_PORT
EOF
  esac

  printf '\n(PREP=1 %s does steps 1-2 for you, unattended.)\n' "$0"
}

# --- measurement ------------------------------------------------------------
# Both servers now STREAM every match, so total time is dominated by transferring
# the body on any query with a lot of results — which would report bandwidth, not
# query speed. So capture both:
#   ttfb  time to first byte: how fast the server starts answering
#   total time to last byte: ttfb plus serialising and shipping the whole result
# On a small result they coincide; on a large one they are the interesting pair.
timings() {
  local i
  for ((i = 0; i < ${N:-20}; i++)); do
    curl -s -o /dev/null --fail --max-time 300 \
      -w '%{time_starttransfer} %{time_total}\n' "$1" 2>/dev/null || true
  done
}
stats() { # $1 = which column (1=ttfb, 2=total)
  awk -v col="$1" 'NF >= 2 { print $col * 1000 }' | sort -n | awk '
    { a[NR] = $1 }
    function pct(p,   i) { i = int((p / 100) * NR + 0.999999); if (i < 1) i = 1; if (i > NR) i = NR; return a[i] }
    END { if (NR == 0) { printf "%-38s", "no successful requests"; exit }
          printf "p50=%8.1f  p95=%8.1f  p99=%8.1f", pct(50), pct(95), pct(99) }'
}
warm() { local i; for ((i = 0; i < WARMUP; i++)); do curl -s -o /dev/null --max-time 300 "$1" 2>/dev/null || true; done; }

# Race one query shape on both servers, reporting ttfb and total side by side.
#
# Iterations scale DOWN with result size. Both servers stream every match, so a hot
# key ships hundreds of megabytes per request; N=30 against the live set would move
# ~10 GB per server and take half an hour to say what five samples already say.
# Small results keep the full N, where per-request noise actually needs averaging
# out. The chosen N is printed with each case so a number is never read as more
# precise than it is.
race() { # $1 = label  $2 = path  $3 = note
  local sraw kraw sc kc n
  sc=$(curl -sf --max-time 600 "$SIEVE_URL$2" 2>/dev/null | jq 'length' 2>/dev/null || echo '?')
  kc=$(curl -sf --max-time 600 "$KUPO_URL$2" 2>/dev/null | jq 'length' 2>/dev/null || echo '?')
  case "$sc" in
    ''|'?') n=3 ;;
    *) if [ "$sc" -gt 20000 ]; then n=3; elif [ "$sc" -gt 2000 ]; then n=8; else n=$N; fi ;;
  esac
  printf '\n%s  (%s)\n' "$1" "$3"
  printf '  rows: sieve=%s kupo=%s%s   [n=%s]\n' "$sc" "$kc" \
    "$([ "$sc" = "$kc" ] && echo '' || echo '  <- DIFFER, times below are not comparable')" "$n"
  warm "$SIEVE_URL$2"; warm "$KUPO_URL$2"
  sraw=$(N=$n timings "$SIEVE_URL$2"); kraw=$(N=$n timings "$KUPO_URL$2")
  printf '  %-5s ttfb  %s\n' "sieve" "$(printf '%s\n' "$sraw" | stats 1)"
  printf '  %-5s ttfb  %s\n' "kupo" "$(printf '%s\n' "$kraw" | stats 1)"
  printf '  %-5s total %s\n' "sieve" "$(printf '%s\n' "$sraw" | stats 2)"
  printf '  %-5s total %s\n' "kupo" "$(printf '%s\n' "$kraw" | stats 2)"
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
