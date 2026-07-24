#!/usr/bin/env bash
#
# Batch-size sweep for cardano-sieve.
#
# Runs sieve over a fixed range (origin..UNTIL_SLOT) at several --batch-size
# values and reports, for each: CPU% (CPU-time / wall-clock — the CPU-bound
# fraction), wall-clock, CPU-time (user+sys), and peak RSS. Purpose: make the
# SQLite commit-vs-WAL-bloat U-curve visible in one table.
#
#   - too-small batches -> many COMMITs -> fsync stalls -> idle -> low CPU%
#   - too-big batches   -> one long txn  -> WAL can't checkpoint, grows huge ->
#                          every read searches a bigger WAL -> more CPU per row
#   - the sweet spot is in between.
#
# Each measurement re-syncs origin..UNTIL_SLOT from the local node into a
# throwaway SQLite db. sieve exits at --until, so /usr/bin/time captures it
# directly. Requires a synced node serving NODE_SOCKET, a built sieve, and GNU
# /usr/bin/time.
#
# Usage:
#   UNTIL_SLOT=2000000 ./bench/batch-sweep.sh
#   BATCHES="1000 50000 200000" UNTIL_SLOT=2000000 ./bench/batch-sweep.sh
#
# Result (measured 2026-07-24 on this box: 8-core, preview, origin..2,000,000;
# single runs, so the exact seconds wobble but the U-shape reproduces):
#
#   batch    CPU%   wall_s   cpu_time_s
#   1000     65%    41.7     27.4
#   10000    85%    35.1     29.9
#   50000    92%    31.6     29.3     <- sweet spot, now the compiled default
#   100000   92%    33.4     30.9
#   200000   95%    41.7     39.7
#
# 50000 is the bottom of the commit-overhead vs WAL-bloat U-curve and is now
# sieve's default batch size (Cardano.Sieve, pBatchSize). No need to re-run this
# sweep unless the write path (Insert.hs) changes.
set -euo pipefail

UNTIL_SLOT="${UNTIL_SLOT:?set UNTIL_SLOT to the target slot, e.g. 2000000}"
NODE_SOCKET="${NODE_SOCKET:-$HOME/node.socket}"
TESTNET_MAGIC="${TESTNET_MAGIC:-2}"
BATCHES="${BATCHES:-1000 10000 50000 100000 200000}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SIEVE_BIN="${SIEVE_BIN:-$(cd "$REPO_ROOT" && cabal list-bin cardano-sieve 2>/dev/null || true)}"
CLI="${CLI:-$HOME/.cabal/bin/cardano-cli}"
TIME_BIN="${TIME_BIN:-/usr/bin/time}"

WORK="$(mktemp -d -t batch-sweep-XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
log() { printf '\033[1;36m[sweep]\033[0m %s\n' "$*"; }

# ---- preflight --------------------------------------------------------------
[ -x "${SIEVE_BIN:-}" ] || { echo "sieve binary not found (set SIEVE_BIN)"; exit 1; }
[ -S "$NODE_SOCKET" ]   || { echo "no node socket at $NODE_SOCKET"; exit 1; }
command -v "$TIME_BIN" >/dev/null || { echo "need GNU /usr/bin/time"; exit 1; }
# A silent no-op run (e.g. the node is down) would poison the whole table, so
# confirm the node actually answers before starting.
if [ -x "$CLI" ]; then
  "$CLI" query tip --testnet-magic "$TESTNET_MAGIC" --socket-path "$NODE_SOCKET" >/dev/null 2>&1 \
    || { echo "node not responding on $NODE_SOCKET — start it first"; exit 1; }
fi

# ---- GNU time -v report parsing ---------------------------------------------
# value after the first ": " on the line matching the given key
field() { awk -F': ' -v k="$1" 'index($0,k){sub(/^[^:]*: */,""); print; exit}' "$2"; }
# "Elapsed (wall clock)" as seconds (m:ss or h:mm:ss)
elapsed_s() {
  awk '/Elapsed \(wall clock\)/ {
    n=split($NF,a,":"); print (n==3) ? a[1]*3600+a[2]*60+a[3] : a[1]*60+a[2]
  }' "$1"
}

log "sieve = $SIEVE_BIN"
log "range origin..$UNTIL_SLOT   batches: $BATCHES"
printf '\n%-9s %6s %9s %11s %9s\n' batch CPU% wall_s cpu_time_s rss_MiB
for b in $BATCHES; do
  db="$WORK/sieve.sqlite3"; tf="$WORK/t.time"
  rm -f "$db" "$db"-wal "$db"-shm
  log "batch $b ..."
  "$TIME_BIN" -v -o "$tf" \
    "$SIEVE_BIN" --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" \
      --database "$db" --since origin --until "$UNTIL_SLOT" --batch-size "$b" \
    >/dev/null 2>>"$WORK/sieve.err" || true
  cpu=$(field 'Percent of CPU this job got' "$tf")
  wall=$(elapsed_s "$tf")
  u=$(field 'User time' "$tf"); s=$(field 'System time' "$tf")
  rss=$(field 'Maximum resident set size' "$tf")
  awk -v b="$b" -v cpu="${cpu:-?}" -v w="${wall:-0}" -v u="${u:-0}" -v s="${s:-0}" -v rss="${rss:-0}" \
    'BEGIN {
       printf "%-9s %6s %9.1f %11.1f %9.0f%s\n", b, cpu, w, u+s, rss/1024,
              (w < 5 ? "   <- suspicious: node blip / failed run?" : "")
     }'
done
