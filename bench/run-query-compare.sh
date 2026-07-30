#!/usr/bin/env bash
#
# Query-latency head-to-head: cardano-sieve vs kupo, over HTTP.
#
# What it does for you:
#   * auto-prepares the cardano-sieve database — syncs to --until UNTIL_SLOT
#     (cached & reused) and builds its query indexes;
#   * checks whether both query servers are up. If not, it PRINTS the exact two
#     commands to run — one per terminal — and exits. Re-run once they're live;
#   * when both are up, hits GET /matches/{addr}?unspent on BOTH (kupo and sieve
#     both accept a base16 address, so the URL is identical) N times and reports
#     p50/p95/p99 (ms) side by side.
#
# The two servers are meant to run in their own terminals (they are long-lived);
# this script never starts or stops them, it just measures against them.
#
# Env (defaults shown):
#   UNTIL_SLOT=2000000  NODE_SOCKET=$HOME/node.socket  TESTNET_MAGIC=2
#   NODE_CONFIG=$HOME/cardano-bin/11.0.1/share/preview/config.json
#   SIEVE_PORT=3000  KUPO_PORT=1442  N=200  WARMUP=10
#   SIEVE_BIN / KUPO_BIN (else discovered via cabal)
#
# CAVEAT (index parity): a fair race needs BOTH tools' query indexes built.
# sieve's are built here (--build-indexes); kupo builds its non-essential indexes
# only on reaching the node's real tip, which a bounded --until sync never does —
# so the script checks kupo's DB for the address index and WARNS if it is absent.
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

# --- prepare the sieve database (one-shot: sync if needed, then build indexes) -
ensure_sieve_db() {
  [ -x "${SIEVE_BIN:-}" ] || die "cardano-sieve binary not found (build it, or set SIEVE_BIN)"
  # [ -s ] guards against sqlite3 creating an empty file just by opening a
  # missing path (which would then look like a 0-row db and re-trigger the sync).
  local have=0
  [ -s "$SIEVE_DB" ] && have=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM unspent" 2>/dev/null || echo 0)
  if [ "${have:-0}" -eq 0 ]; then
    log "syncing sieve db origin..$UNTIL_SLOT (one-time) ..."
    [ -S "$NODE_SOCKET" ] || die "no node socket at $NODE_SOCKET"
    mkdir -p "$(dirname "$SIEVE_DB")"
    local tmp="$SIEVE_DB.partial"; rm -f "$tmp" "$tmp"-wal "$tmp"-shm
    "$SIEVE_BIN" --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" \
      --database "$tmp" --since origin --until "$UNTIL_SLOT" >/dev/null 2>&1 \
      || { rm -f "$tmp" "$tmp"-wal "$tmp"-shm; die "sieve sync failed — is cardano-node running and serving $NODE_SOCKET?"; }
    mv "$tmp" "$SIEVE_DB"; [ -f "$tmp-wal" ] && mv "$tmp-wal" "$SIEVE_DB-wal" || true
  else
    log "reusing sieve db ($have unspent rows)"
  fi
  log "building sieve query indexes (idempotent) ..."
  "$SIEVE_BIN" --socket-path /tmp/unused.sock --testnet-magic "$TESTNET_MAGIC" \
    --database "$SIEVE_DB" --build-indexes >/dev/null
}

# --- is each server up? -----------------------------------------------------
sieve_up() { curl -sf -o /dev/null --max-time 3 "$SIEVE_URL/matches/00?unspent" 2>/dev/null; }
kupo_slot() { curl -sf --max-time 3 "$KUPO_URL/health" 2>/dev/null | jq -r '.most_recent_checkpoint // 0' 2>/dev/null || echo 0; }
kupo_up() { local s; s=$(kupo_slot); [ "${s:-0}" -ge "$UNTIL_SLOT" ] 2>/dev/null; }

# --- print the commands to run in two terminals, then exit ------------------
print_setup() {
  mkdir -p "$KUPO_DIR"
  cat <<EOF

Both servers must be running. Start each in its OWN terminal, then re-run this
script — it will detect them and measure.

  ── terminal 1: kupo (needs cardano-node running; syncs to $UNTIL_SLOT, then serves) ──
  '$KUPO_BIN' --node-socket '$NODE_SOCKET' --node-config '$NODE_CONFIG' \\
    --since origin --until $UNTIL_SLOT --match '*' \\
    --workdir '$KUPO_DIR' --host 127.0.0.1 --port $KUPO_PORT

  ── terminal 2: cardano-sieve query server ──
  '$SIEVE_BIN' --socket-path /tmp/unused.sock --testnet-magic $TESTNET_MAGIC \\
    --database '$SIEVE_DB' --serve $SIEVE_PORT

Wait until kupo's /health 'most_recent_checkpoint' reaches $UNTIL_SLOT
(curl -s $KUPO_URL/health | jq .most_recent_checkpoint), then run this again.
EOF
}

# --- measurement ------------------------------------------------------------
timings() { local i; for ((i = 0; i < N; i++)); do curl -s -o /dev/null --fail --max-time 30 -w '%{time_total}\n' "$1" 2>/dev/null || true; done | awk 'NF { printf "%.3f\n", $1 * 1000 }'; }
stats() {
  sort -n | awk '
    { a[NR] = $1 }
    function pct(p,   i) { i = int((p / 100) * NR + 0.999999); if (i < 1) i = 1; if (i > NR) i = NR; return a[i] }
    END { if (NR == 0) { print "no successful requests"; exit }
          printf "p50=%.1f  p95=%.1f  p99=%.1f  max=%.1f (ms)  n=%d\n", pct(50), pct(95), pct(99), a[NR], NR }'
}
warm() { local i; for ((i = 0; i < WARMUP; i++)); do curl -s -o /dev/null --max-time 30 "$1" 2>/dev/null || true; done; }

# --- run --------------------------------------------------------------------
ensure_sieve_db

if ! sieve_up || ! kupo_up; then
  sieve_up && log "sieve server: up" || log "sieve server: DOWN"
  kupo_up  && log "kupo server: up"  || log "kupo server: DOWN (need slot >= $UNTIL_SLOT, have $(kupo_slot))"
  print_setup
  exit 0
fi
log "both servers up — sieve=$SIEVE_URL  kupo=$KUPO_URL"

# Fairness check: does kupo have an address index?
KUPO_SQLITE="$KUPO_DIR/kupo.sqlite3"
if [ -f "$KUPO_SQLITE" ] && ! sqlite3 "$KUPO_SQLITE" "SELECT name FROM sqlite_master WHERE type='index'" 2>/dev/null | grep -qi address; then
  log "WARNING: kupo has no address index (bounded sync never hit real tip) — kupo numbers are unfairly slow."
fi

# Case: the busiest address as base16 — accepted by both servers.
ADDR=$(sqlite3 "$SIEVE_DB" "SELECT hex(address) FROM unspent GROUP BY address ORDER BY count(*) DESC LIMIT 1")
Q="/matches/$ADDR?unspent"
log "case: unspent by address ${ADDR:0:16}…  (N=$N, warmup=$WARMUP)"

# Sanity: both should return a similar number of rows.
sc=$(curl -sf --max-time 30 "$SIEVE_URL$Q" 2>/dev/null | jq 'length' 2>/dev/null || echo '?')
kc=$(curl -sf --max-time 30 "$KUPO_URL$Q"  2>/dev/null | jq 'length' 2>/dev/null || echo '?')
log "result rows: sieve=$sc  kupo=$kc  $([ "$sc" = "$kc" ] || echo '(DIFFER — check encoding/parity)')"

warm "$SIEVE_URL$Q"; warm "$KUPO_URL$Q"
printf '\n%-6s %s\n' "sieve" "$(timings "$SIEVE_URL$Q" | stats)"
printf '%-6s %s\n'   "kupo"  "$(timings "$KUPO_URL$Q"  | stats)"
