#!/usr/bin/env bash
#
# run-testnet-sieve.sh
#
# Phase-1 proof-of-life driver: spin up a local cardano-testnet, then run
# cardano-sieve against it and watch block numbers stream out.
#
#   ./scripts/run-testnet-sieve.sh
#
# Run until you Ctrl-C (the testnet is torn down on exit). Set SIEVE_DURATION
# to a number of seconds to run non-interactively (used for quick verification):
#
#   SIEVE_DURATION=45 ./scripts/run-testnet-sieve.sh
#
# Env overrides:
#   CARDANO_NODE_REPO   checkout with built cardano-node/cardano-testnet
#                       (default: ~/repos/work/cardano-node)
#   TESTNET_MAGIC       network magic (default: 42)

set -euo pipefail

# --- configuration ----------------------------------------------------------

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CARDANO_NODE_REPO="${CARDANO_NODE_REPO:-$HOME/repos/work/cardano-node}"
TESTNET_MAGIC="${TESTNET_MAGIC:-42}"

log() { printf '\033[1;36m[sieve-test]\033[0m %s\n' "$*"; }
die() { printf '\033[1;31m[sieve-test] ERROR:\033[0m %s\n' "$*" >&2; exit 1; }

# --- locate the built cardano binaries --------------------------------------
# cardano-testnet invokes cardano-node/cardano-cli by name; it honours the
# CARDANO_NODE / CARDANO_CLI env vars, with a PATH fallback. We set both.

DIST="$CARDANO_NODE_REPO/dist-newstyle"
CARDANO_NODE="$(find "$DIST" -type f -path '*/build/cardano-node/cardano-node' 2>/dev/null | head -1)"
CARDANO_TESTNET="$(find "$DIST" -type f -path '*/build/cardano-testnet/cardano-testnet' 2>/dev/null | head -1)"
CARDANO_CLI="$(command -v cardano-cli || true)"

[ -x "${CARDANO_NODE:-}" ]    || die "cardano-node not found under $DIST (build it, or set CARDANO_NODE_REPO)"
[ -x "${CARDANO_TESTNET:-}" ] || die "cardano-testnet not found under $DIST"
[ -x "${CARDANO_CLI:-}" ]     || die "cardano-cli not found on PATH"

export CARDANO_NODE CARDANO_CLI
export PATH="$(dirname "$CARDANO_NODE"):$(dirname "$CARDANO_CLI"):$PATH"

log "cardano-node    : $CARDANO_NODE"
log "cardano-testnet : $CARDANO_TESTNET"
log "cardano-cli     : $CARDANO_CLI"

# --- build cardano-sieve -----------------------------------------------------

log "building cardano-sieve ..."
( cd "$REPO_ROOT" && cabal build exe:cardano-sieve -j4 >/dev/null )
SIEVE_BIN="$(cd "$REPO_ROOT" && cabal list-bin exe:cardano-sieve)"
[ -x "$SIEVE_BIN" ] || die "could not locate built cardano-sieve binary"
log "cardano-sieve   : $SIEVE_BIN"

# --- working dir (kept short: unix socket paths cap at ~104 chars) -----------

RUN_DIR="$(mktemp -d /tmp/cs-tn.XXXXXX)"
TN_OUT="$RUN_DIR/net"          # cardano-testnet --output-dir (it creates this)
TN_LOG="$RUN_DIR/testnet.log"

# --- cleanup -----------------------------------------------------------------

cleanup() {
  trap - EXIT INT TERM
  [ -n "${SIEVE_PID:-}" ] && kill "$SIEVE_PID" 2>/dev/null || true
  if [ -n "${TN_PID:-}" ]; then
    log "stopping testnet (pid $TN_PID) ..."
    kill -INT "$TN_PID" 2>/dev/null || true
    for _ in $(seq 1 8); do kill -0 "$TN_PID" 2>/dev/null || break; sleep 1; done
  fi
  # backstop: kill any node still bound to our workdir
  pkill -f "$TN_OUT" 2>/dev/null || true
  log "done. testnet log + genesis left in: $RUN_DIR"
}
trap cleanup EXIT INT TERM

# --- start the testnet -------------------------------------------------------

log "starting cardano-testnet (magic $TESTNET_MAGIC) — this takes ~30-90s to first block"
log "  testnet log: $TN_LOG"
( cd "$RUN_DIR" && "$CARDANO_TESTNET" cardano \
    --testnet-magic "$TESTNET_MAGIC" \
    --output-dir "$TN_OUT" \
    >"$TN_LOG" 2>&1 ) &
TN_PID=$!

# --- wait for a node socket to appear ----------------------------------------

log "waiting for a node socket under $TN_OUT ..."
SOCKET=""
for _ in $(seq 1 180); do
  if ! kill -0 "$TN_PID" 2>/dev/null; then
    tail -n 40 "$TN_LOG" >&2 || true
    die "cardano-testnet exited early — see $TN_LOG"
  fi
  # '|| true': under 'set -o pipefail' find can exit non-zero mid-traversal
  # while the testnet is still writing its dir tree, and 'head -1' closing the
  # pipe once sockets appear gives 'sort' a SIGPIPE — either would abort the
  # script under 'set -e'. We only care about the captured path.
  SOCKET="$(find "$TN_OUT" -type s -name sock 2>/dev/null | sort | head -1 || true)"
  if [ -n "$SOCKET" ]; then break; fi
  sleep 1
done
[ -n "$SOCKET" ] || die "no node socket appeared within 180s — see $TN_LOG"
log "node socket: $SOCKET"

# give the node a moment to start accepting node-to-client connections
sleep 3

# --- run the sieve -----------------------------------------------------------

DB="$RUN_DIR/headers.db"
log "running cardano-sieve — headers -> $DB, block numbers follow (Ctrl-C to stop):"
echo "--------------------------------------------------------------------------"
if [ -n "${SIEVE_DURATION:-}" ]; then
  timeout "${SIEVE_DURATION}" "$SIEVE_BIN" \
    --socket-path "$SOCKET" --testnet-magic "$TESTNET_MAGIC" --database "$DB" &
  SIEVE_PID=$!
  wait "$SIEVE_PID" || true
else
  "$SIEVE_BIN" \
    --socket-path "$SOCKET" --testnet-magic "$TESTNET_MAGIC" --database "$DB" &
  SIEVE_PID=$!
  wait "$SIEVE_PID" || true
fi

# --- report what landed in the database --------------------------------------

echo "--------------------------------------------------------------------------"
if command -v sqlite3 >/dev/null 2>&1 && [ -f "$DB" ]; then
  rows="$(sqlite3 "$DB" 'SELECT count(*) FROM block_header;' 2>/dev/null || echo '?')"
  span="$(sqlite3 "$DB" 'SELECT COALESCE(min(block_no),-1)||".."||COALESCE(max(block_no),-1) FROM block_header;' 2>/dev/null || echo '?')"
  log "block_header rows: $rows (block_no range: $span)"
  log "sample: $(sqlite3 -header -column "$DB" 'SELECT block_no, slot_no, substr(hash,1,16)||"..." AS hash FROM block_header ORDER BY block_no DESC LIMIT 3;' 2>/dev/null | tr '\n' '|')"
else
  log "(install sqlite3 to see a DB summary; db file: $DB)"
fi
