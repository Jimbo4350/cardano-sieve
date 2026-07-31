#!/usr/bin/env bash
#
# Sync benchmark: cardano-sieve vs kupo over a fixed preview block range.
#
# Both tools replay the SAME immutable range origin..UNTIL_SLOT from the SAME
# local node socket, indexing every output (wildcard). We measure wall-clock
# time, CPU-time and CPU% (= cpu/wall; >100% means multiple cores were busy),
# peak RSS, and final on-disk DB size, RUNS times each, and report the median.
# The node's tip may keep advancing past UNTIL_SLOT — irrelevant, since
# a bounded historical range replays deterministically.
#
# Requires: a preview cardano-node synced past UNTIL_SLOT serving NODE_SOCKET;
# built cardano-sieve and kupo; GNU /usr/bin/time, curl, jq, sqlite3, stat.
#
# UNTIL_SLOT defaults to 2000000 — a range with real tx volume (the first ~200k
# preview slots are nearly empty, so they measure startup, not indexing). A
# discarded warm-up sync runs first so cold disk cache doesn't skew the timed
# runs, each run idles COOLDOWN seconds first so the CPU doesn't thermally
# throttle (which otherwise drifts later runs slower), and scratch DBs are
# reclaimed as they go so they don't evict the node's warm cache. Override
# anything via env, e.g.:
#   UNTIL_SLOT=4000000 RUNS=5 COOLDOWN=30 ./bench/run-sync-bench.sh
#
# Fairness notes (validated against both codebases):
#   - Index parity: sieve's --until run does NOT build its deferred secondary
#     indexes, so kupo runs with --defer-db-indexes; both then measure pure
#     ingest. Index-build cost is a separate, one-time measurement (sieve does
#     not wire installDeferredIndexes to the CLI yet).
#   - Prune parity: neither prunes (kupo runs without --prune-utxo), so both
#     keep full history — matching sieve's append-only model.
#   - Storage-pragma asymmetry (NOT equalised): both use WAL + synchronous=NORMAL
#     + foreign_keys=ON, but kupo sets page_size=32768 / cache_size=1024 while
#     sieve is on SQLite defaults (4K pages, ~2 MiB cache). Flatters neither
#     cleanly; read part of any gap as sieve's untuned-storage headroom.
set -euo pipefail

NODE_SOCKET="${NODE_SOCKET:-$HOME/node.socket}"
TESTNET_MAGIC="${TESTNET_MAGIC:-2}"
NODE_CONFIG="${NODE_CONFIG:-$HOME/cardano-bin/11.0.1/share/preview/config.json}"
UNTIL_SLOT="${UNTIL_SLOT:-2000000}"
RUNS="${RUNS:-3}"
COOLDOWN="${COOLDOWN:-15}"   # seconds idle before each run so the CPU doesn't thermally throttle
KUPO_PORT="${KUPO_PORT:-1442}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
KUPO_REPO="${KUPO_REPO:-$HOME/repos/kupo}"

# Build both tools from source BEFORE locating their binaries, so we never
# benchmark a stale build (`cabal list-bin` only *finds* a binary — it does not
# rebuild it, so a code change like the --until fix would otherwise be missed).
# `cabal build` is a no-op when nothing changed, so repeat runs stay fast.
# Set BUILD=0 to skip, or pin SIEVE_BIN / KUPO_BIN to bypass build+locate for
# that tool. The exe: prefix is required — bare `cardano-sieve` is ambiguous
# (lib vs exe).
BUILD="${BUILD:-1}"
if [ "$BUILD" = 1 ]; then
  [ -n "${SIEVE_BIN:-}" ] || ( cd "$REPO_ROOT" && cabal build exe:cardano-sieve -j4 ) \
    || { echo "FATAL: building cardano-sieve failed" >&2; exit 1; }
  [ -n "${KUPO_BIN:-}" ]  || ( cd "$KUPO_REPO"  && cabal build exe:kupo -j4 ) \
    || { echo "FATAL: building kupo failed (pin KUPO_BIN or set BUILD=0)" >&2; exit 1; }
fi
SIEVE_BIN="${SIEVE_BIN:-$(cd "$REPO_ROOT" && cabal list-bin exe:cardano-sieve 2>/dev/null || true)}"
KUPO_BIN="${KUPO_BIN:-$(cd "$KUPO_REPO"  && cabal list-bin exe:kupo 2>/dev/null || true)}"
TIME_BIN="${TIME_BIN:-/usr/bin/time}"
CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"

WORK="$(mktemp -d -t sync-bench-XXXXXX)"
trap 'rm -rf "$WORK"; pkill -INT -f "kupo .*--port $KUPO_PORT" 2>/dev/null || true' EXIT
log() { printf '\033[1;36m[bench]\033[0m %s\n' "$*"; }

# ---- preflight --------------------------------------------------------------
[ -x "${SIEVE_BIN:-}" ] || { echo "cardano-sieve binary not found (set SIEVE_BIN)"; exit 1; }
[ -x "${KUPO_BIN:-}" ]  || { echo "kupo binary not found (set KUPO_BIN)"; exit 1; }
[ -f "$NODE_CONFIG" ]   || { echo "no node config at $NODE_CONFIG"; exit 1; }
for t in "$TIME_BIN" "$CARDANO_CLI" timeout curl jq sqlite3 stat; do
  command -v "$t" >/dev/null || { echo "need $t on PATH (override via env)"; exit 1; }
done

# A live node is non-negotiable, and a socket file existing is NOT proof of one
# (it can be stale after a crash). So: (1) require the socket, (2) actually query
# the node, (3) confirm its tip has passed UNTIL_SLOT so the whole range is
# replayable. Every failure is loud and tells you how to fix it.
[ -S "$NODE_SOCKET" ] || {
  echo "FATAL: no node socket at $NODE_SOCKET — is cardano-node running?" >&2
  echo "  start one with:  $REPO_ROOT/bench/run-node.sh" >&2
  echo "  (or point elsewhere with NODE_SOCKET=/path/to/node.socket)" >&2
  exit 1
}
tip_json="$(timeout "${NODE_QUERY_TIMEOUT:-30}" "$CARDANO_CLI" query tip \
              --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" 2>&1)" || {
  echo "FATAL: socket $NODE_SOCKET exists but no node answered 'query tip' in time." >&2
  echo "  the node is down, unresponsive, or the socket is stale. cardano-cli said:" >&2
  printf '%s\n' "$tip_json" | sed 's/^/    /' >&2
  echo "  (re)start it with:  $REPO_ROOT/bench/run-node.sh" >&2
  exit 1
}
tip_slot="$(printf '%s' "$tip_json" | jq -r '.slot // empty')"
[ -n "$tip_slot" ] || { echo "FATAL: could not parse a slot from 'query tip': $tip_json" >&2; exit 1; }
if [ "$tip_slot" -lt "$UNTIL_SLOT" ]; then
  echo "FATAL: node tip slot $tip_slot is behind UNTIL_SLOT=$UNTIL_SLOT." >&2
  echo "  the range origin..$UNTIL_SLOT is not on chain yet — let the node keep syncing" >&2
  echo "  (bench/run-node.sh) until 'query tip' reports slot >= $UNTIL_SLOT, or lower UNTIL_SLOT." >&2
  exit 1
fi
log "node live on $NODE_SOCKET, tip slot $tip_slot >= UNTIL_SLOT $UNTIL_SLOT"

# GNU time -v: pull elapsed seconds + max RSS (KiB) out of the report.
max_rss_kb() { awk '/Maximum resident set size/ {print $NF}' "$1"; }
elapsed_s() {
  awk '/Elapsed \(wall clock\)/ {
    n=split($NF,a,":"); print (n==3) ? a[1]*3600+a[2]*60+a[3] : a[1]*60+a[2]
  }' "$1"
}

# Total on-disk size of a SQLite db: the main file plus any WAL/SHM sidecars.
# Measuring only the main file undercounts a db whose WAL hasn't been
# checkpointed (missing files are simply skipped).
db_bytes() { # $1 = path to the .sqlite3 file
  { stat -c%s "$1"     2>/dev/null
    stat -c%s "$1-wal" 2>/dev/null
    stat -c%s "$1-shm" 2>/dev/null
  } | awk '{s+=$1} END{print s+0}'
}

# CPU-time (user+system, seconds) — the actual work done, independent of
# wall-clock. sieve's comes from GNU time -v; kupo's from /proc (see run_kupo).
cpu_time_s() { awk -F': ' '/User time/{u=$2} /System time/{s=$2} END{printf "%.2f", u+s}' "$1"; }
# Clock ticks per second, to convert /proc/<pid>/stat utime+stime into seconds.
CLK_TCK=$(getconf CLK_TCK 2>/dev/null || echo 100)

# ---- one sieve run: it exits at --until, so time(1) captures it directly ----
run_sieve() { # $1 = run index -> "elapsed_s max_rss_kb db_bytes cpu_time_s"
  local db="$WORK/sieve-$1.sqlite3" tf="$WORK/sieve-$1.time"
  local sl="$WORK/sieve-$1.synclog"
  # Progress goes to a file rather than the terminal: it is one heartbeat line
  # every 5 s, so the write is immaterial to the measurement, but it keeps this
  # run's stdout clean and gives a timed run something to watch.
  #   tail -f "$sl"
  "$TIME_BIN" -v -o "$tf" \
    "$SIEVE_BIN" \
      --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" \
      --database "$db" --since origin --until "$UNTIL_SLOT" >"$sl" 2>&1
  echo "$(elapsed_s "$tf") $(max_rss_kb "$tf") $(db_bytes "$db") $(cpu_time_s "$tf")"
}

# ---- one kupo run: kupo keeps running after --until, so we poll /health and
#      stop it once it reports the target slot. --------------------------------
run_kupo() { # $1 = run index -> "elapsed_s max_rss_kb db_bytes cpu_time_s"
  local dir="$WORK/kupo-$1" start end slot rss_kb
  mkdir -p "$dir"
  start=$(date +%s.%N)
  # Index parity: sieve's bounded (--until) run never builds its deferred
  # secondary indexes, so kupo must skip its non-essential indexes too — else
  # kupo is charged for index work sieve doesn't do. --defer-db-indexes does
  # that; both still create primary keys + the essential unique index.
  #
  # NOTE: deliberately no /usr/bin/time wrapper here. kupo has to be *killed*
  # (it doesn't exit at --until), and GNU time reports a garbage Max RSS when
  # its child is signalled. Instead we launch kupo directly (so $! is kupo's own
  # pid) and read its peak RSS from the kernel — VmHWM in /proc, in KB, the same
  # metric time's Max RSS reports, but reliable for a killed process.
  "$KUPO_BIN" \
      --node-socket "$NODE_SOCKET" --node-config "$NODE_CONFIG" \
      --since origin --until "$UNTIL_SLOT" --match '*' --workdir "$dir" \
      --defer-db-indexes \
      --host 127.0.0.1 --port "$KUPO_PORT" >/dev/null 2>&1 &
  local pid=$!
  # kupo does NOT exit at --until; it keeps serving. So poll /health and stop it
  # once it reaches the bound. Blocks don't fall on every slot, so the last
  # checkpoint settles just BELOW --until (e.g. 1999983 for a 2000000 bound) and
  # never equals it — waiting for ">= UNTIL_SLOT" hangs forever. Instead detect
  # that indexing has FINISHED: the checkpoint stops advancing. Record 'end' at
  # the moment progress stops (not after the confirmation polls) so kupo is not
  # charged for the detection wait. Verified against kupo's Health.hs:
  # most_recent_checkpoint is a bare slot number, so this jq is correct.
  local prev="" stable=0 seen=0
  end=""
  while kill -0 "$pid" 2>/dev/null; do
    sleep 1
    slot=$(curl -sf -H 'Accept: application/json' \
             "http://127.0.0.1:$KUPO_PORT/health" \
             | jq -r '.most_recent_checkpoint // empty' 2>/dev/null || true)
    [ -n "$slot" ] || continue
    if [ "$slot" -ge "$UNTIL_SLOT" ] 2>/dev/null; then end=$(date +%s.%N); break; fi
    if [ "$slot" != "$prev" ]; then
      seen=1; stable=0; prev="$slot"          # still making progress
    elif [ "$seen" -eq 1 ]; then
      stable=$((stable + 1))
      [ "$stable" -eq 1 ] && end=$(date +%s.%N)   # progress just stopped: finish time
      [ "$stable" -ge 3 ] && break                # held steady 3 polls: confirmed done
    fi
  done
  [ -n "$end" ] || end=$(date +%s.%N)
  # VmHWM is the kernel's peak-RSS-so-far (monotonic), so a single read before
  # shutdown captures the whole run's peak, in KB.
  rss_kb=$(awk '/^VmHWM:/ {print $2}' "/proc/$pid/status" 2>/dev/null || echo 0)
  # CPU-time across ALL kupo threads: utime+stime from /proc/<pid>/stat (fields
  # 14 & 15, in clock ticks), read before killing. comm (field 2) can contain
  # spaces/parens, so slice from after the last ") " and index from there.
  local kcpu=0 statline rest; local -a f
  if [ -r "/proc/$pid/stat" ]; then
    statline=$(cat "/proc/$pid/stat"); rest=${statline##*') '}
    read -ra f <<<"$rest" || true
    kcpu=$(awk -v u="${f[11]:-0}" -v st="${f[12]:-0}" -v hz="$CLK_TCK" 'BEGIN{printf "%.2f",(u+st)/hz}')
  fi
  # Graceful shutdown (SIGINT, not SIGKILL) so kupo checkpoints its WAL and the
  # on-disk db size is real rather than stranded in the -wal sidecar.
  kill -INT "$pid" 2>/dev/null || true; wait "$pid" 2>/dev/null || true
  awk -v s="$start" -v e="$end" -v r="${rss_kb:-0}" -v db="$(db_bytes "$dir/kupo.sqlite3")" -v cpu="${kcpu:-0}" \
      'BEGIN { printf "%.2f %s %s %s\n", e - s, r, db, cpu }'
}

median() { sort -n | awk '{a[NR]=$1} END{ print (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2 }'; }

# One timed run of a tool; appends its (elapsed, rss, db) to that tool's files.
one_run() { # $1=name $2=runner $3=run-index
  local name="$1" fn="$2" i="$3" el rss db cpu pct
  # Idle before each timed run so a CPU heated by the previous run (or the
  # warm-up) clocks back up; without this, thermal throttling makes later runs
  # slower and drifts the median upward.
  if [ "${COOLDOWN:-0}" -gt 0 ]; then log "cooldown ${COOLDOWN}s ..."; sleep "$COOLDOWN"; fi
  log "$name run $i/$RUNS ..."
  read -r el rss db cpu < <("$fn" "$i")
  echo "$el" >>"$WORK/$name.el"; echo "$rss" >>"$WORK/$name.rss"
  echo "$db" >>"$WORK/$name.db"; echo "$cpu" >>"$WORK/$name.cpu"
  pct=$(awk -v c="$cpu" -v w="$el" 'BEGIN{ printf "%.0f", (w>0)? c/w*100 : 0 }')
  log "  -> ${el}s wall, ${cpu}s cpu (${pct}% CPU), $((rss / 1024)) MiB RSS, $((db / 1024 / 1024)) MiB db"
  # Reclaim this run's scratch DB so ~1.5 GB of them don't accumulate in the
  # page cache and evict the node's warm immutable-chunk cache (which would
  # re-cold later runs). Keep run 1 — the correctness gate reads it.
  if [ "$i" != 1 ]; then
    rm -f "$WORK/$name-$i.sqlite3"* 2>/dev/null || true   # sieve: sieve-N.sqlite3[-wal/-shm]
    rm -rf "$WORK/$name-$i" 2>/dev/null || true           # kupo: kupo-N/ workdir
  fi
}

log "range origin..$UNTIL_SLOT (wildcard), $RUNS runs each, interleaved"
log "sieve = $SIEVE_BIN"
log "kupo  = $KUPO_BIN"
for name in sieve kupo; do : >"$WORK/$name.el"; : >"$WORK/$name.rss"; : >"$WORK/$name.db"; : >"$WORK/$name.cpu"; done
# A stray kupo from an aborted earlier run can still hold KUPO_PORT, making a
# fresh kupo fail to bind — it exits instantly and shows up as a 0-row run.
# Clear the port before starting.
if ss -ltn 2>/dev/null | grep -q "127.0.0.1:$KUPO_PORT "; then
  log "port $KUPO_PORT busy — clearing a stray kupo before starting"
  pkill -INT -f "kupo .*--port $KUPO_PORT" 2>/dev/null || true
  sleep 2
fi
# Warm-up: the first sync of a range is COLD — the node reads the immutable
# chunks off disk into an empty OS page cache, so it serves blocks slower (this
# is why the first 2M run was 58s vs ~40s once warm). One discarded sync warms
# that shared cache, so every timed run below is warm and the interleaving
# doesn't hand one tool a cold run and the other a warm one.
log "warm-up sync (discarded) ..."
run_sieve warmup >/dev/null 2>&1 || true
rm -f "$WORK"/sieve-warmup.*
# Interleave sieve/kupo per iteration so neither systematically benefits from a
# node/OS page cache the other just warmed. The median over RUNS still cancels
# per-run jitter.
for i in $(seq 1 "$RUNS"); do
  one_run sieve run_sieve "$i"
  one_run kupo  run_kupo  "$i"
done

# ---- correctness gate: a perf number is meaningless if the two tools indexed
#      different data. Compare the indexed UTxO set from run 1 of each. --------
log "verifying both tools indexed the same UTxO set ..."
s_all=$(sqlite3 "$WORK/sieve-1.sqlite3"      'SELECT count(*) FROM outputs' 2>/dev/null || echo '?')
s_uns=$(sqlite3 "$WORK/sieve-1.sqlite3"      'SELECT count(*) FROM unspent' 2>/dev/null || echo '?')
k_all=$(sqlite3 "$WORK/kupo-1/kupo.sqlite3"  'SELECT count(*) FROM inputs' 2>/dev/null || echo '?')
k_uns=$(sqlite3 "$WORK/kupo-1/kupo.sqlite3"  'SELECT count(*) FROM inputs WHERE spent_at IS NULL' 2>/dev/null || echo '?')
log "  outputs-ever: sieve=$s_all kupo=$k_all | unspent: sieve=$s_uns kupo=$k_uns"
if [ "$s_all" != "$k_all" ] || [ "$s_uns" != "$k_uns" ]; then
  log "  WARNING: counts differ — the tools are NOT doing equal work; the numbers below are not comparable."
fi

printf '\n%-7s %9s %9s %7s %13s %11s\n' tool wall_s cpu_s CPU% peak_RSS_MiB db_MiB
for name in sieve kupo; do
  mw=$(median <"$WORK/$name.el"); mc=$(median <"$WORK/$name.cpu")
  printf '%-7s %9.1f %9.1f %6.0f%% %13.0f %11.0f\n' \
    "$name" "$mw" "$mc" \
    "$(awk -v c="$mc" -v w="$mw" 'BEGIN{print (w>0)? c/w*100 : 0}')" \
    "$(awk -v r="$(median <"$WORK/$name.rss")" 'BEGIN{print r/1024}')" \
    "$(awk -v d="$(median <"$WORK/$name.db")" 'BEGIN{print d/1024/1024}')"
done
