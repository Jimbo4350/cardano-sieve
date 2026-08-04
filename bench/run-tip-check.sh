#!/usr/bin/env bash
#
# Does the indexer commit per block once it is at the tip?
#
# Flush-on-idle (ADR-020 Decision 2) exists so that a query against a
# tip-following sieve sees the chain as of the last block, rather than as of the
# last --batch-size boundary. Committing only on a row count would leave an open
# transaction for hours at the tip, because blocks arrive every ~20s carrying a
# handful of matched rows and 50,000 is never reached.
#
# bench/run-flush-check.sh answers the other half — that the same mechanism costs
# nothing during bulk sync. This one answers whether it actually fires.
#
# THE TRICK: --since <current tip>. Sieve starts already caught up, so this takes
# three minutes instead of a full multi-million-slot sync.
#
# Usage:
#   ./bench/run-tip-check.sh              # ~3 minutes
#   TICKS=20 ./bench/run-tip-check.sh     # watch longer
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
NODE_SOCKET="${NODE_SOCKET:-$HOME/node.socket}"
TESTNET_MAGIC="${TESTNET_MAGIC:-2}"
TICKS="${TICKS:-9}"
INTERVAL="${INTERVAL:-20}"
DB="${DB:-/tmp/sieve-tip-check.sqlite}"
LOG="${LOG:-/tmp/sieve-tip-check.log}"

[ -S "$NODE_SOCKET" ] || {
  echo "FATAL: no node socket at $NODE_SOCKET — start one with ./bench/run-node.sh" >&2
  exit 1
}

SIEVE="${SIEVE_BIN:-}"
if [ -z "$SIEVE" ]; then
  ( cd "$REPO_ROOT" && cabal build exe:cardano-sieve -j4 >/dev/null 2>&1 )
  SIEVE="$(cd "$REPO_ROOT" && cabal list-bin exe:cardano-sieve)"
fi
[ -x "$SIEVE" ] || { echo "FATAL: no cardano-sieve binary (set SIEVE_BIN)" >&2; exit 1; }

tip="$(timeout 30 cardano-cli query tip \
  --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" 2>&1)" || {
  echo "FATAL: socket exists but no node answered 'query tip'." >&2; exit 1; }
slot="$(echo "$tip" | sed -n 's/.*"slot": *\([0-9]*\).*/\1/p')"
hash="$(echo "$tip" | sed -n 's/.*"hash": *"\([0-9a-f]*\)".*/\1/p')"
progress="$(echo "$tip" | sed -n 's/.*"syncProgress": *"\([0-9.]*\)".*/\1/p')"

[ -n "$slot" ] && [ -n "$hash" ] || { echo "FATAL: could not read tip from: $tip" >&2; exit 1; }
case "$progress" in
  100*) ;;
  *) echo "WARNING: node syncProgress is $progress, not 100 — it is still catching up," >&2
     echo "         so blocks may arrive in bursts rather than one per ~${INTERVAL}s." >&2 ;;
esac

rm -f "$DB" "$DB"-wal "$DB"-shm
echo "starting at tip $slot"
echo "watching for $((TICKS * INTERVAL))s, sampling every ${INTERVAL}s"
echo

"$SIEVE" --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" \
  --database "$DB" --since "$slot.$hash" >"$LOG" 2>&1 &
sieve_pid=$!
# Kill the indexer however this exits, including Ctrl-C.
trap 'kill "$sieve_pid" 2>/dev/null || true' EXIT INT TERM

count() { sqlite3 "$DB" "SELECT count(*) FROM $1" 2>/dev/null || echo 0; }

prev=-1
committed_ticks=0
for _ in $(seq 1 "$TICKS"); do
  sleep "$INTERVAL"
  kill -0 "$sieve_pid" 2>/dev/null || { echo "sieve exited early — see $LOG"; tail -5 "$LOG"; exit 1; }
  b=$(count blocks)
  # Only counts as evidence if it CHANGED: a static non-zero number means one
  # early commit and nothing since, which is the failure this is looking for.
  if [ "$prev" -ge 0 ] && [ "$b" -gt "$prev" ]; then
    committed_ticks=$((committed_ticks + 1))
    delta="+$((b - prev))"
  else
    delta="  "
  fi
  printf '%s  blocks %-6s %s\n' "$(date +%T)" "$b" "$delta"
  prev=$b
done

echo
echo "=== sieve log ==="
cat "$LOG"
echo
echo "=== VERDICT ==="
if [ "$committed_ticks" -ge 2 ]; then
  echo "PASS — the committed block count advanced in $committed_ticks of $((TICKS - 1)) samples."
  echo "       Flush-on-idle is firing at the tip: each block is committed as it"
  echo "       arrives, so a query sees the chain as of the last block."
elif [ "$prev" -gt 0 ]; then
  echo "SUSPECT — $prev blocks are committed but the count never advanced."
  echo "          Something committed once and then stopped. Check the log above for"
  echo "          whether blocks were still arriving."
else
  echo "FAIL — nothing was ever committed."
  echo "       If the log shows blocks arriving, the batch is being held open and"
  echo "       flush-on-idle is not firing. If it shows no blocks, the node was not"
  echo "       producing any during the window — re-run for longer with TICKS=30."
fi
