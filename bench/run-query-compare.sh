#!/usr/bin/env bash
#
# Query-latency head-to-head: cardano-sieve vs kupo, over HTTP.
#
# For each query shape we send the SAME logical lookup to both servers N times,
# measure per-request latency with curl, and report p50 / p95 / p99 (ms) side by
# side. This is the query half of the "beat kupo" comparison (the sync half is
# run-sync-bench.sh).
#
# ── What you need first ──────────────────────────────────────────────────────
# Two servers already running, each over a database synced to the SAME block
# range (so the same keys exist in both):
#
#   sieve:  cardano-sieve --database <synced.sqlite> --serve 3000
#   kupo :  kupo --workdir <synced-dir> --node-socket <sock> --node-config <cfg> \
#                --host 127.0.0.1 --port 1442
#           (kupo keeps serving queries; it does not need to be at tip)
#
# ── How a "case" works ───────────────────────────────────────────────────────
# The two servers encode the same query differently in the URL (today sieve
# takes the address as base16; kupo takes bech32), so every case carries a path
# for EACH server. Add cases by editing the CASES array below, one per line:
#
#     "label | <sieve path> | <kupo path>"
#
# e.g.  "by-address | /matches/<ADDR_HEX>?unspent | /matches/<ADDR_BECH32>?unspent"
#
# A single default case can also be supplied via env (SIEVE_PATH / KUPO_PATH)
# without editing the file.
#
# ── Caveats (kept deliberately simple) ───────────────────────────────────────
#  * Each request is a fresh curl (new TCP + HTTP connection). That connection
#    cost is included and is paid equally by both tools, so the RELATIVE
#    comparison holds; absolute numbers are a touch higher than a keep-alive
#    client (wrk/hey) would show. Swapping in such a tool is a later refinement.
#  * No response-equality check yet — this measures latency only. Confirming
#    both return the same UTxOs is a TODO once sieve's response shape matches
#    kupo's.
#
# Env (defaults shown):
#   SIEVE_URL=http://127.0.0.1:3000   KUPO_URL=http://127.0.0.1:1442
#   N=200   (timed requests per server per case)   WARMUP=10 (discarded)
set -euo pipefail

SIEVE_URL="${SIEVE_URL:-http://127.0.0.1:3000}"
KUPO_URL="${KUPO_URL:-http://127.0.0.1:1442}"
N="${N:-200}"
WARMUP="${WARMUP:-10}"

# Cases: "label | sieve path | kupo path". Fill in real keys for your range.
CASES=(
  # "by-address | /matches/<ADDR_HEX>?unspent | /matches/<ADDR_BECH32>?unspent"
)
# Convenience: a single case straight from env, no file edit needed.
if [ -n "${SIEVE_PATH:-}" ] && [ -n "${KUPO_PATH:-}" ]; then
  CASES+=("${LABEL:-env} | $SIEVE_PATH | $KUPO_PATH")
fi

command -v curl >/dev/null || { echo "need curl on PATH"; exit 1; }
log() { printf '\033[1;36m[qcmp]\033[0m %s\n' "$*"; }

# Warn (don't fail) if a server looks down, so the error is obvious up front.
reachable() { curl -sS -o /dev/null --max-time 3 "$1" 2>/dev/null; }
reachable "$KUPO_URL/health" || log "WARNING: kupo not reachable at $KUPO_URL/health"

# Send GET $1 N times; emit each request's total time in MILLISECONDS, one per
# line. Failed requests (--fail: non-2xx, or connect error) are dropped, not
# counted as fast 0 ms.
timings() {
  local url="$1" i
  for ((i = 0; i < N; i++)); do
    curl -s -o /dev/null --fail --max-time 30 -w '%{time_total}\n' "$url" 2>/dev/null || true
  done | awk 'NF { printf "%.3f\n", $1 * 1000 }'
}

# Read ms values on stdin -> "p50=.. p95=.. p99=.. max=.. (ms) n=.." (nearest-rank).
stats() {
  sort -n | awk '
    { a[NR] = $1 }
    function pct(p,   i) { i = int((p / 100) * NR + 0.999999); if (i < 1) i = 1; if (i > NR) i = NR; return a[i] }
    END {
      if (NR == 0) { print "no successful requests"; exit }
      printf "p50=%.1f  p95=%.1f  p99=%.1f  max=%.1f (ms)  n=%d\n", pct(50), pct(95), pct(99), a[NR], NR
    }'
}

# A couple of untimed requests to warm caches/connections before measuring.
warm() { local i; for ((i = 0; i < WARMUP; i++)); do curl -s -o /dev/null --max-time 30 "$1" 2>/dev/null || true; done; }

if [ ${#CASES[@]} -eq 0 ]; then
  log "no cases defined — add lines to CASES, or set SIEVE_PATH and KUPO_PATH."
  log "example: SIEVE_PATH=/matches/<hex>?unspent KUPO_PATH=/matches/<bech32>?unspent $0"
  exit 0
fi

log "sieve=$SIEVE_URL  kupo=$KUPO_URL  N=$N  warmup=$WARMUP"
for case in "${CASES[@]}"; do
  # Split "label | sieve_path | kupo_path" on '|', trimming surrounding spaces.
  IFS='|' read -r label spath kpath <<<"$case"
  label="${label#"${label%%[![:space:]]*}"}"; label="${label%"${label##*[![:space:]]}"}"
  spath="$(echo "$spath" | xargs)"; kpath="$(echo "$kpath" | xargs)"

  printf '\n\033[1;36m── %s ──\033[0m\n' "$label"
  warm "$SIEVE_URL$spath"; warm "$KUPO_URL$kpath"
  printf '   sieve  %s\n' "$(timings "$SIEVE_URL$spath" | stats)"
  printf '   kupo   %s\n' "$(timings "$KUPO_URL$kpath"  | stats)"
done
