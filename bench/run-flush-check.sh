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

log() { printf '%s  %s\n' "$(date +%H:%M:%S)" "$*"; }

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
    ( cd "$dir" && cabal build exe:cardano-sieve -j4 >/dev/null 2>&1 )
    cp "$(cd "$dir" && cabal list-bin exe:cardano-sieve)" "$out"
    git -C "$REPO_ROOT" worktree remove --force "$dir"
  fi
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
    >"$WORK/$2.log" 2>&1
  t1=$(date +%s.%N)
  echo "$t1 $t0" | awk '{printf "%.2f", $1-$2}'
}

median() { sort -n | awk '{a[NR]=$1} END{print (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2}'; }

measure() { # $1 = binary, $2 = name; prints "median|all"
  local i t all=()
  for i in $(seq 1 "$WARMUP"); do
    t=$(one_run "$1" "$2-warmup-$i"); log "  $2 warmup $i: ${t}s (discarded)"
  done
  for i in $(seq 1 "$RUNS"); do
    t=$(one_run "$1" "$2-$i"); all+=("$t"); log "  $2 run $i: ${t}s"
  done
  printf '%s|%s' "$(printf '%s\n' "${all[@]}" | median)" "${all[*]}"
}

# ---- run ------------------------------------------------------------------

log "building HEAD"
head_bin=$(build_at HEAD)

if [ -n "$BASELINE_REF" ]; then
  log "building baseline $BASELINE_REF"
  base_bin=$(build_at "$BASELINE_REF")
fi

log "measuring HEAD (flush-on-idle)"
head_res=$(measure "$head_bin" head)
head_med=${head_res%%|*}

echo
echo "================ RESULT ================"
printf 'HEAD (flush-on-idle)   median %ss   [%s]\n' "$head_med" "${head_res#*|}"

if [ -n "$BASELINE_REF" ]; then
  log "measuring baseline $BASELINE_REF (count cap only)"
  base_res=$(measure "$base_bin" base)
  base_med=${base_res%%|*}
  printf 'BASE %-17s median %ss   [%s]\n' "$BASELINE_REF" "$base_med" "${base_res#*|}"
  echo
  awk -v h="$head_med" -v b="$base_med" 'BEGIN{
    d = h - b; p = (b > 0) ? 100*d/b : 0;
    printf "delta  %+.2fs  (%+.1f%%)\n\n", d, p;
    if (p <= 3)       print "VERDICT: no measurable cost. The peek stays quiet during bulk sync.";
    else if (p <= 10) print "VERDICT: small but real cost. Worth a second look at how often the peek fires.";
    else              print "VERDICT: flush-on-idle IS firing during bulk sync. It needs a floor\n         (e.g. only flush on idle when pending rows exceed some minimum).";
  }'
fi

echo
echo "logs: $WORK"
