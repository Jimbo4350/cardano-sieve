#!/usr/bin/env bash
#
# Does flush-on-idle cost bulk-sync throughput?
#
# ADR-020 Decision 2 commits the open batch whenever the ChainSync pipeline
# drains, instead of only when --batch-size rows have accumulated. At the tip
# that is the point: every block commits, so queries see fresh data. During bulk
# sync it is supposed to be invisible, because the pipeline should never be empty
# — the node is streaming blocks faster than we can decode them.
#
# "Supposed to be" is the part this measures. If the node ever starves us for a
# full pipeline's worth of requests, the idle flush fires during bulk sync and
# commits per block, which is the expensive end of the batch-size curve
# (bench/batch-sweep.sh: 41.7s at batch=1000 versus 31.6s at batch=50000 over
# origin..2,000,000).
#
# WHY IT A/Bs RATHER THAN COMPARING TO A RECORDED NUMBER
#
# A stored baseline is not trustworthy across days on this box. A previous
# "regression" here turned out to be the machine, not the code — the same commit
# measured 59.4s once and 29.6s another time. So BASELINE_REF is built and timed
# in the same session, interleaved with HEAD, and only the difference is
# reported.
#
# FIRST RUNS ARE ALWAYS SLOW. The node's page cache starts cold; observed
# 57s / 49s / 40s on three consecutive identical runs. WARMUP runs are executed
# and discarded before any timing is kept.
#
# Usage:
#   ./bench/run-flush-check.sh                        # A/B HEAD vs the pre-flush commit
#   RUNS=8 ./bench/run-flush-check.sh                 # more samples
#   BASELINE_REF=abc1234 ./bench/run-flush-check.sh   # A/B against something else
#   BASELINE_REF= ./bench/run-flush-check.sh          # HEAD only, no A/B
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
NODE_SOCKET="${NODE_SOCKET:-$HOME/node.socket}"
TESTNET_MAGIC="${TESTNET_MAGIC:-2}"
UNTIL_SLOT="${UNTIL_SLOT:-2000000}"
RUNS="${RUNS:-5}"
WARMUP="${WARMUP:-2}"
WORK="${WORK:-$(mktemp -d)}"
# 907cb3c is the commit immediately before flush-on-idle (a3ea66f).
BASELINE_REF="${BASELINE_REF-907cb3c}"

# To stderr, not stdout: 'measure' below is consumed by command substitution, so
# anything log() writes to stdout ends up inside the result string instead of on
# the terminal. (It did, on the first run — the reported median was a timestamp.)
log() { printf '%s  %s\n' "$(date +%H:%M:%S)" "$*" >&2; }

# ---- preflight ------------------------------------------------------------

[ -S "$NODE_SOCKET" ] || {
  echo "FATAL: no node socket at $NODE_SOCKET — start one with ./bench/run-node.sh" >&2
  exit 1
}

tip_json="$(timeout 30 cardano-cli query tip \
  --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" 2>&1)" || {
  echo "FATAL: socket exists but no node answered 'query tip'." >&2
  exit 1
}
tip_slot="$(echo "$tip_json" | sed -n 's/.*"slot": *\([0-9]*\).*/\1/p')"
[ "${tip_slot:-0}" -ge "$UNTIL_SLOT" ] || {
  echo "FATAL: node tip $tip_slot is behind UNTIL_SLOT=$UNTIL_SLOT." >&2
  exit 1
}

# A concurrent build or sync makes every number here meaningless.
if pgrep -f "ghc-9|cabal build" >/dev/null 2>&1; then
  echo "WARNING: a GHC/cabal build is running — timings will be noisy." >&2
  echo "         Ctrl-C now, or accept the noise." >&2
  sleep 5
fi

log "node tip $tip_slot, indexing origin..$UNTIL_SLOT"
log "$WARMUP warmup run(s) discarded, then $RUNS timed run(s) per variant"
log "work dir $WORK"

# ---- build one variant, return its binary path ----------------------------

# The baseline is built in a throwaway WORKTREE, never by checking out an old
# commit in place. Two reasons: a checkout would delete this script (it does not
# exist in the older commit) while bash is still reading it, and it would fight
# any uncommitted work in the tree.
build_at() { # $1 = git ref or "HEAD"; prints a binary path
  local ref="$1" out="$WORK/sieve-$1" dir

  if [ "$ref" = "HEAD" ]; then
    ( cd "$REPO_ROOT" && cabal build exe:cardano-sieve -j4 >/dev/null 2>&1 )
    cp "$(cd "$REPO_ROOT" && cabal list-bin exe:cardano-sieve)" "$out"
  else
    dir="$WORK/wt-$ref"
    git -C "$REPO_ROOT" worktree add -q --detach "$dir" "$ref"
    # Untracked but required: without it the source-repository-package pin does
    # not resolve and the worktree will not build.
    [ -f "$REPO_ROOT/cabal.project.local" ] && cp "$REPO_ROOT/cabal.project.local" "$dir/"
    # The SRP is pinned to a commit that is NOT reachable from the upstream
    # default branch, so a fresh clone cannot fetch it ("Could not parse object
    # f202918f..."). The parent tree already holds a clone containing it; reuse
    # that rather than going to the network.
    if [ -d "$REPO_ROOT/dist-newstyle/src" ]; then
      mkdir -p "$dir/dist-newstyle"
      cp -r "$REPO_ROOT/dist-newstyle/src" "$dir/dist-newstyle/"
    fi
    ( cd "$dir" && cabal build exe:cardano-sieve -j4 >"$WORK/build-$ref.log" 2>&1 ) || {
      echo "FATAL: baseline $ref failed to build; see $WORK/build-$ref.log" >&2
      tail -20 "$WORK/build-$ref.log" >&2
      exit 1
    }
    local bin
    bin="$(cd "$dir" && cabal list-bin exe:cardano-sieve 2>/dev/null || true)"
    [ -x "$bin" ] || { echo "FATAL: no baseline binary for $ref" >&2; exit 1; }
    cp "$bin" "$out"
    git -C "$REPO_ROOT" worktree remove --force "$dir"
  fi
  [ -x "$out" ] || { echo "FATAL: no binary built for $ref" >&2; exit 1; }
  echo "$out"
}

# ---- time one sync --------------------------------------------------------

one_run() { # $1 = binary, $2 = label; prints wall seconds
  local db="$WORK/db.sqlite"
  rm -f "$db" "$db"-wal "$db"-shm
  local t0 t1
  t0=$(date +%s.%N)
  "$1" --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" \
    --database "$db" --since origin --until "$UNTIL_SLOT" \
    >"$WORK/$2.log" 2>&1 || {
    echo "FATAL: sync failed ($2); see $WORK/$2.log" >&2
    tail -5 "$WORK/$2.log" >&2
    exit 1
  }
  t1=$(date +%s.%N)
  # A run that indexed nothing is not a fast run, it is a broken one.
  grep -q "sync done" "$WORK/$2.log" || {
    echo "FATAL: $2 produced no 'sync done' line; see $WORK/$2.log" >&2
    exit 1
  }
  echo "$t1 $t0" | awk '{printf "%.2f", $1-$2}'
}

median() { sort -n | awk '{a[NR]=$1} END{print (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2}'; }

spread() { sort -n | awk '{a[NR]=$1} END{ printf "%.2f", (a[1] > 0) ? a[NR]/a[1] : 0 }'; }

# ---- run ------------------------------------------------------------------

log "building HEAD"
head_bin=$(build_at HEAD)

if [ -n "$BASELINE_REF" ]; then
  log "building baseline $BASELINE_REF"
  base_bin=$(build_at "$BASELINE_REF")
fi

# INTERLEAVED, not variant-by-variant. Running all of A then all of B charges any
# drift in machine conditions to whichever variant happened to be running at the
# time — which is exactly what went wrong the first time this script produced a
# number: the node finished its own chain catch-up partway through the baseline
# block, and three of its five runs landed at 53-63s against 34s for the other
# two. Alternating spreads that kind of drift across both variants instead.
head_times=(); base_times=()

for i in $(seq 1 "$WARMUP"); do
  t=$(one_run "$head_bin" "head-warmup-$i"); log "  warmup $i head: ${t}s (discarded)"
  if [ -n "$BASELINE_REF" ]; then
    t=$(one_run "$base_bin" "base-warmup-$i"); log "  warmup $i base: ${t}s (discarded)"
  fi
done

for i in $(seq 1 "$RUNS"); do
  t=$(one_run "$head_bin" "head-$i"); head_times+=("$t"); log "  round $i  head: ${t}s"
  if [ -n "$BASELINE_REF" ]; then
    t=$(one_run "$base_bin" "base-$i"); base_times+=("$t"); log "  round $i  base: ${t}s"
  fi
done

head_med=$(printf '%s\n' "${head_times[@]}" | median)
head_spread=$(printf '%s\n' "${head_times[@]}" | spread)

echo
echo "================ RESULT ================"
printf 'HEAD (flush-on-idle)   median %ss  max/min %s  [%s]\n' \
  "$head_med" "$head_spread" "${head_times[*]}"

if [ -n "$BASELINE_REF" ]; then
  base_med=$(printf '%s\n' "${base_times[@]}" | median)
  base_spread=$(printf '%s\n' "${base_times[@]}" | spread)
  printf 'BASE %-17s median %ss  max/min %s  [%s]\n' \
    "$BASELINE_REF" "$base_med" "$base_spread" "${base_times[*]}"
  echo
  # Judge the PAIRED difference, not the absolute times.
  #
  # This box is shared with a desktop and a cardano-node, so absolute run times
  # wander — observed medians of 32.5s and 37.4s for the same two binaries
  # fifteen minutes apart. Gating on absolute spread therefore reports
  # INCONCLUSIVE forever, which it did. Each round runs both variants
  # back-to-back, so head_i - base_i cancels whatever the machine was doing at
  # the time, and it is the scatter of THOSE that says whether the answer is
  # trustworthy.
  deltas=()
  for i in $(seq 0 $((RUNS - 1))); do
    deltas+=("$(awk -v a="${head_times[$i]}" -v b="${base_times[$i]}" 'BEGIN{printf "%.2f", a-b}')")
  done
  delta_med=$(printf '%s\n' "${deltas[@]}" | median)
  printf 'paired deltas (head - base)  [%s]  median %+.2fs\n\n' "${deltas[*]}" "$delta_med"

  awk -v dm="$delta_med" -v b="$base_med" -v n="$RUNS" \
    -v ds="$(printf '%s\n' "${deltas[@]}" | tr '\n' ' ')" 'BEGIN{
    p = (b > 0) ? 100*dm/b : 0;
    # How many paired rounds favoured each variant. Splitting near half and half
    # is what "no difference" looks like; a consistent sign is a real effect.
    split(ds, d, " "); pos = 0; tot = 0;
    for (i in d) { if (d[i] != "") { tot++; if (d[i] > 0) pos++ } }
    printf "median paired delta  %+.2fs  (%+.1f%% of base)   %d/%d rounds slower\n\n", dm, p, pos, tot;
    # Deliberately generic. This started as a flush-on-idle check and is now the
    # A/B harness for any two commits (BASELINE_REF), so it must not name a
    # cause it cannot know — it reports that HEAD is slower, not why.
    if (p > 10)      printf "VERDICT: HEAD is %.1f%% SLOWER. That is a real regression, not noise.\n", p;
    else if (p > 3)  printf "VERDICT: HEAD is %.1f%% slower. Small but likely real.\n", p;
    else if (p < -10) printf "VERDICT: HEAD is %.1f%% FASTER. A real improvement.\n", -p;
    else if (pos == tot || pos == 0)
                     printf "VERDICT: delta is small (%+.1f%%) but every round agreed in sign,\n         so it is probably real and probably not worth acting on.\n", p;
    else             print "VERDICT: no measurable difference. Rounds disagree on sign, so what\n         is left is noise.";
  }'
fi

echo
echo "logs: $WORK"
