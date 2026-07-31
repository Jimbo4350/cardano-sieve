#!/usr/bin/env bash
#
# Step-1 query-latency suite (database only — no HTTP server involved).
#
# What it does: for each kind of lookup sieve must answer, it runs a realistic
# query against a synced cardano-sieve database many times and reports how long
# it took — once WITHOUT a helper index and once WITH one — so we can see which
# indexes are actually worth building.
#
# ---------------------------------------------------------------------------
# A short SQLite primer (skip if you already know this)
# ---------------------------------------------------------------------------
# * The data lives in one SQLite file. The main table is `unspent`. A query with
#   no index forces SQLite to read EVERY row to find matches — a "full table
#   scan". That is slow, and gets slower as the table grows.
# * An INDEX is a pre-sorted lookup structure — like the index at the back of a
#   book — that lets SQLite jump straight to the matching rows instead of reading
#   them all. It makes reads faster but costs a little on every write.
# * `EXPLAIN QUERY PLAN` asks SQLite to DESCRIBE how it would run a query (which
#   index it would use, whether it must sort) WITHOUT actually running it. This
#   script prints that description for every case. The phrases to recognise:
#     - "SCAN <table>"                   SQLite is reading every row (no index).
#     - "SEARCH <table> USING INDEX x"   it jumps straight to matches via index x
#                                        (this is the good case).
#     - "USE TEMP B-TREE FOR ORDER BY"   after finding the rows, SQLite had to do
#                                        a SEPARATE sorting pass to honour our
#                                        "newest first" ORDER BY. That pass is
#                                        avoidable: an index on BOTH the filter
#                                        column AND the sort column (e.g.
#                                        (address, created_slot)) stores the rows
#                                        already in sorted order, so SQLite reads
#                                        them in order and — because we ask for
#                                        only one page (LIMIT) — stops early.
#                                        When that happens this line disappears
#                                        and the query gets dramatically faster.
# ---------------------------------------------------------------------------
#
# Per case it prints:
#     what    : the client's question, in plain English
#     why     : what this case is probing
#     kupo    : the equivalent kupo endpoint (the real query this stands in for)
#     matches : how many rows the probed key hits (this changes the answer a lot)
#     sql     : the exact query (the long X'..' byte value is shortened to X'…')
#     no-index / indexed : latency (min/median/p95/max) + the query plan
#
# This measures the DATABASE only (find + fetch + sort). A real request also pays
# HTTP + JSON on top — that is a later, server-level measurement.
#
# Each query filters on one specific value — e.g. WHERE address = <this address>.
# That value is the query's "target". For each dimension the script auto-picks
# the target that appears in the MOST rows (the busiest key): the worst case for
# an index. A normal address/holder has far fewer rows and only comes out faster.
#
# The synced database is handled for you: a cached copy is reused; otherwise a
# one-time bounded sync (origin..UNTIL_SLOT) builds it. Only that first sync needs
# the node and the sieve binary — after that it just queries the cached file.
#
# Env (all optional; defaults shown):
#   SIEVE_DB=$XDG_CACHE_HOME/cardano-sieve-bench/preview-until-<UNTIL_SLOT>.sqlite
#   UNTIL_SLOT=4000000  NODE_SOCKET=$HOME/node.socket  TESTNET_MAGIC=2
#   SIEVE_BIN=<cabal list-bin cardano-sieve>  N=50  PAGE=100
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
NODE_SOCKET="${NODE_SOCKET:-$HOME/node.socket}"
TESTNET_MAGIC="${TESTNET_MAGIC:-2}"
UNTIL_SLOT="${UNTIL_SLOT:-4000000}"
SIEVE_BIN="${SIEVE_BIN:-$(cd "$REPO_ROOT" && cabal list-bin cardano-sieve 2>/dev/null || true)}"
CACHE_DIR="${CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/cardano-sieve-bench}"
SIEVE_DB="${SIEVE_DB:-$CACHE_DIR/preview-until-$UNTIL_SLOT.sqlite}"
N="${N:-50}"
PAGE="${PAGE:-100}"

# The columns a client gets back for a match. Present on both the `unspent` and
# `outputs` tables; `payment_credential`/`delegation_credential` exist only on
# `unspent`. (`value` is a raw-bytes blob holding the encoded coin/asset amounts.)
COLS="output_reference, address, value, datum_hash, reference_script_hash, created_slot"
ORDER="ORDER BY created_slot DESC LIMIT $PAGE"

command -v sqlite3 >/dev/null || { echo "need sqlite3 on PATH"; exit 1; }
# log: print a cyan status line.  hr: print a cyan divider that heads each case.
log() { printf '\033[1;36m[qbench]\033[0m %s\n' "$*"; }
hr()  { printf '\n\033[1;36m── %s ──\033[0m\n' "$*"; }

# Succeeds if the database at $1 exists and its `unspent` table has at least one
# row — i.e. it is a real synced db, not empty or half-built. Used to decide
# whether a cached db can be reused.
db_has_rows() { local n; n=$(sqlite3 "$1" "SELECT count(*) FROM unspent" 2>/dev/null || echo 0); [ "${n:-0}" -gt 0 ]; }

# Make sure $SIEVE_DB is a usable synced database: reuse the cached file if it
# already holds data, otherwise run cardano-sieve once to index origin..UNTIL_SLOT
# into it. Syncs to a ".partial" file and renames only on success, so an
# interrupted sync never leaves a half-built db that later looks ready.
ensure_db() {
  if [ -f "$SIEVE_DB" ] && db_has_rows "$SIEVE_DB"; then log "reusing synced db: $SIEVE_DB"; return; fi
  log "no synced db at $SIEVE_DB — syncing origin..$UNTIL_SLOT (one-time) ..."
  [ -x "${SIEVE_BIN:-}" ] || { echo "cardano-sieve binary not found; build it (cabal build cardano-sieve -j4) or set SIEVE_BIN"; exit 1; }
  [ -S "$NODE_SOCKET" ]   || { echo "no node socket at $NODE_SOCKET (needed for the one-time sync)"; exit 1; }
  mkdir -p "$(dirname "$SIEVE_DB")"
  local tmp="$SIEVE_DB.partial"; rm -f "$tmp" "$tmp-wal" "$tmp-shm"
  "$SIEVE_BIN" --socket-path "$NODE_SOCKET" --testnet-magic "$TESTNET_MAGIC" \
    --database "$tmp" --since origin --until "$UNTIL_SLOT" >/dev/null
  db_has_rows "$tmp" || { echo "sync produced an empty db — check the node/range"; exit 1; }
  mv "$tmp" "$SIEVE_DB"
  # SQLite's write-ahead-log mode can leave .wal/.shm side-files next to the db;
  # move them alongside so the cached database is complete.
  if [ -f "$tmp-wal" ]; then mv "$tmp-wal" "$SIEVE_DB-wal"; fi
  if [ -f "$tmp-shm" ]; then mv "$tmp-shm" "$SIEVE_DB-shm"; fi
  log "synced -> $SIEVE_DB"
}

# Addresses, credentials and ids are stored as raw bytes (a BLOB column), not
# text. To match one in a query we write it as X'<hex>' — SQLite's byte-literal
# syntax — so hex() turns the stored bytes into that hex text. This picks the
# value that appears in the most rows: the heaviest, worst-case lookup.
pick_hex() { sqlite3 "$SIEVE_DB" "SELECT hex($1) FROM $2 WHERE $1 IS NOT NULL GROUP BY $1 ORDER BY count(*) DESC LIMIT 1"; }

# We time a query by handing it to the sqlite3 command-line N times and reading
# how long each run took. Each statement must end in ';' or sqlite3 glues the N
# lines together into one broken statement (our query strings omit the ';').
gen() { local i; for ((i = 0; i < N; i++)); do printf '%s;\n' "$1"; done; }
# ".timer on" makes the sqlite3 CLI print a "Run Time: real <seconds> ..." line
# after each statement; we run the query N times and pull out those seconds.
timings() { { echo ".timer on"; gen "$1"; } | sqlite3 "$SIEVE_DB" 2>&1 | awk '/Run Time: real/ {print $4}'; }
# Reduce a list of per-run seconds (one per line) to min/median/p95/max, in ms.
stats() {
  sort -n | awk '{a[NR]=$1} END {
    if (NR==0) { print "no timings"; exit }
    p=int(0.95*NR); if(p<1)p=1
    m=(NR%2)?a[(NR+1)/2]:(a[NR/2]+a[NR/2+1])/2
    printf "min=%.2f median=%.2f p95=%.2f max=%.2f (ms)\n", a[1]*1000, m*1000, a[p]*1000, a[NR]*1000
  }'
}

# Print the descriptive block for a case. Args: what why kupo query matches
header() {
  local disp; disp=$(printf '%s' "$4" | sed "s/X'[0-9A-Fa-f]*'/X'…'/g")
  printf '   what    : %s\n'                 "$1"
  printf '   why     : %s\n'                 "$2"
  printf '   kupo    : %s\n'                 "$3"
  printf '   matches : %s row(s) for the probed key\n' "$5"
  printf '   sql     : %s\n'                 "$disp"
}

# Measure ONE variant: print its latency, then how SQLite ran it. Args: mode query
# `EXPLAIN QUERY PLAN` returns SQLite's description of the query (indexes, sorts)
# without running it; we flatten its little tree into one line.
emit() {
  printf '     %-10s' "$1"; timings "$2" | stats
  printf '                plan: '
  sqlite3 "$SIEVE_DB" "EXPLAIN QUERY PLAN $2" | sed '1d; s/^[^A-Za-z]*//' | paste -sd';' - | sed 's/;/ | /g'
}

# Run the SAME query twice so the index's effect is visible: once with the
# candidate index dropped (baseline), once with it created. Args: idx_name idx_def query
measure() {
  if [ -n "$1" ]; then sqlite3 "$SIEVE_DB" "DROP INDEX IF EXISTS $1;"; fi
  emit "no-index" "$3"
  if [ -n "$2" ]; then
    sqlite3 "$SIEVE_DB" "$2;"; emit "indexed" "$3"; sqlite3 "$SIEVE_DB" "DROP INDEX IF EXISTS $1;"
  fi
}

# A point dimension on `unspent`. Tests the COMPOSITE (col, created_slot) index —
# the shape declared in Schema.hs — so the index covers both the WHERE filter and
# the newest-first sort. Args: label col idx what why kupo
dim() {
  local tgt; tgt=$(pick_hex "$2" unspent)
  hr "$1"
  if [ -z "$tgt" ]; then printf '   (no non-null %s values in range; skipped)\n' "$2"; return; fi
  local m; m=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM unspent WHERE $2 = X'$tgt'")
  local q="SELECT $COLS FROM unspent WHERE $2 = X'$tgt' $ORDER"
  header "$4" "$5" "$6" "$q" "$m"
  measure "$3" "CREATE INDEX $3 ON unspent($2, created_slot)" "$q"
}

ensure_db
rows=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM unspent")
maxslot=$(sqlite3 "$SIEVE_DB" "SELECT max(created_slot) FROM unspent"); winlo=$((maxslot - 10000))
log "db=$SIEVE_DB  unspent_rows=$rows  N=$N  PAGE=$PAGE  slot_window=[$winlo,$maxslot]"
log "each case: no-index vs indexed latency; watch the plan for SCAN vs SEARCH"
log "and 'USE TEMP B-TREE FOR ORDER BY' (index doesn't cover the newest-first sort)."

dim "unspent by address" address unspentByAddress \
  "All unspent UTxOs at one address, newest-first page — the wallet/explorer lookup." \
  "Does an address filter index still pay off once results are also sorted by recency?" \
  "GET /matches/{address}?unspent&order=most_recent_first"

dim "unspent by payment_credential" payment_credential unspentByPaymentCredential \
  "All unspent UTxOs under one payment credential (every address sharing that key), newest first." \
  "Same as address, but a credential fans across many addresses — higher cardinality." \
  "GET /matches/{payment_credential}/*?unspent"

dim "unspent by delegation_credential" delegation_credential unspentByDelegationCredential \
  "All unspent UTxOs delegated to one stake credential, newest first." \
  "A lower-cardinality key — does the single-column index win here where address didn't?" \
  "GET /matches/*/{stake_credential}?unspent"

dim "unspent by transaction_id" transaction_id unspentByTransactionId \
  "All unspent outputs produced by one transaction." \
  "A tx has few outputs — confirm the index gives a clean, cheap point lookup." \
  "GET /matches/*@{transaction_id}?unspent"

# payment + delegation: a full base-address match on BOTH credentials at once.
# The two hex values are packed with a '|' and split back out.
pd=$(sqlite3 "$SIEVE_DB" "SELECT hex(payment_credential) || '|' || hex(delegation_credential) FROM unspent WHERE payment_credential IS NOT NULL AND delegation_credential IS NOT NULL GROUP BY payment_credential, delegation_credential ORDER BY count(*) DESC LIMIT 1")
hr "unspent by payment+delegation"
if [ -n "$pd" ]; then
  pcred=${pd%%|*}; dcred=${pd##*|}
  pdq="SELECT $COLS FROM unspent WHERE payment_credential = X'$pcred' AND delegation_credential = X'$dcred' $ORDER"
  pdm=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM unspent WHERE payment_credential = X'$pcred' AND delegation_credential = X'$dcred'")
  header "All unspent UTxOs at a specific base address — both the payment and stake key." \
         "A two-credential predicate: does one composite over both creds + slot serve it in a single seek?" \
         "GET /matches/{payment_credential}/{stake_credential}?unspent" "$pdq" "$pdm"
  measure "unspentByPaymentAndDelegation" "CREATE INDEX unspentByPaymentAndDelegation ON unspent(payment_credential, delegation_credential, created_slot)" "$pdq"
else
  printf '   (no base addresses with both credentials in range; skipped)\n'
fi

# output_reference is the primary key: point lookup, always indexed, no candidate.
oref=$(sqlite3 "$SIEVE_DB" "SELECT hex(output_reference) FROM unspent LIMIT 1")
hr "unspent by output_reference (PK)"
header "One specific unspent output (txid#index)." \
       "Primary-key point lookup — should be free with no extra index. The baseline." \
       "GET /matches/{output_index}@{transaction_id}?unspent" \
       "SELECT $COLS FROM unspent WHERE output_reference = X'$oref'" "1"
measure "" "" "SELECT $COLS FROM unspent WHERE output_reference = X'$oref'"

# policy_id joins the policies index back to unspent. policies stores the small
# policy_ids surrogate rather than the 28-byte hash, so the query resolves the
# hash to its policy_num through one seek on policy_ids(policy_id) first — a
# single-value scalar subquery, which keeps the equality on the leading index
# column. policies carries created_slot, so ORDER BY p.created_slot lets the
# composite cover the sort.
POLNUM="SELECT policy_num FROM policy_ids WHERE policy_id"
pol=$(sqlite3 "$SIEVE_DB" "SELECT hex(d.policy_id) FROM policies p \
JOIN policy_ids d ON d.policy_num = p.policy_num \
GROUP BY p.policy_num ORDER BY count(*) DESC LIMIT 1" 2>/dev/null || true)
hr "unspent by policy_id (join policies)"
if [ -n "$pol" ]; then
  polq="SELECT u.output_reference, u.address, u.value, u.datum_hash, u.reference_script_hash, u.created_slot \
FROM unspent u JOIN policies p ON p.output_reference = u.output_reference \
WHERE p.policy_num = ($POLNUM = X'$pol') ORDER BY p.created_slot DESC LIMIT $PAGE"
  polm=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM policies WHERE policy_num = ($POLNUM = X'$pol')")
  header "All unspent UTxOs holding any asset of one policy, newest first (via the policies join)." \
         "Does policies(policy_num, created_slot) remove the sort and speed up the hot-policy join?" \
         "GET /matches/{policy_id}.*?unspent" "$polq" "$polm"
  measure "policiesByPolicyId" "CREATE INDEX policiesByPolicyId ON policies(policy_num, created_slot)" "$polq"
else
  printf '   (no policies rows in range; skipped)\n'
fi

# asset_id joins on BOTH the policy surrogate and asset_name. Only the POLICY is
# interned, so asset_name stays inline and this keeps a two-column equality on the
# composite index's leading columns. (Interning the (policy, name) PAIR instead
# would collapse this to one integer, but then the policy-only case above becomes
# `policy_num IN (…)` — N disjoint ranges, no covered sort, materialise-and-sort.)
# The policy hash and asset name are packed with a '|' and split back out;
# asset_name may be empty (X'') — a valid, common case.
pa=$(sqlite3 "$SIEVE_DB" "SELECT hex(d.policy_id) || '|' || hex(p.asset_name) FROM policies p \
JOIN policy_ids d ON d.policy_num = p.policy_num \
GROUP BY p.policy_num, p.asset_name ORDER BY count(*) DESC LIMIT 1" 2>/dev/null || true)
hr "unspent by asset_id (policy + asset name)"
if [ -n "$pa" ]; then
  apol=${pa%%|*}; aname=${pa##*|}
  aq="SELECT u.output_reference, u.address, u.value, u.datum_hash, u.reference_script_hash, u.created_slot \
FROM unspent u JOIN policies p ON p.output_reference = u.output_reference \
WHERE p.policy_num = ($POLNUM = X'$apol') AND p.asset_name = X'$aname' ORDER BY p.created_slot DESC LIMIT $PAGE"
  am=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM policies WHERE policy_num = ($POLNUM = X'$apol') AND asset_name = X'$aname'")
  header "All unspent UTxOs holding one specific native asset (policy + asset name), newest first." \
         "Does the composite policies(policy_num, asset_name, created_slot) serve it in one indexed seek?" \
         "GET /matches/{policy_id}.{asset_name}?unspent" "$aq" "$am"
  measure "policiesByAssetId" "CREATE INDEX policiesByAssetId ON policies(policy_num, asset_name, created_slot)" "$aq"
else
  printf '   (no policies rows in range; skipped)\n'
fi

hr "unspent by created_slot window"
cq="SELECT $COLS FROM unspent WHERE created_slot >= $winlo $ORDER"
cm=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM unspent WHERE created_slot >= $winlo")
header "Unspent UTxOs created in a recent slot window — a page of recent activity." \
       "Can one created_slot index serve BOTH the range filter and the newest-first order?" \
       "GET /matches/*?unspent&created_after=$winlo&created_before=$maxslot" "$cq" "$cm"
measure "unspentByCreatedSlot" "CREATE INDEX unspentByCreatedSlot ON unspent(created_slot)" "$cq"

hr "wildcard, most-recent page"
wq="SELECT $COLS FROM unspent $ORDER"
header "The most-recent page of unspent UTxOs across everything (no filter)." \
       "Pure ordering test — this is kupo's #194 slot-index hot path." \
       "GET /matches/*?unspent&order=most_recent_first" "$wq" "$rows"
measure "unspentByCreatedSlot" "CREATE INDEX unspentByCreatedSlot ON unspent(created_slot)" "$wq"

hr "outputs (spent-inclusive) by address"
oaddr=$(pick_hex address outputs)
oq="SELECT $COLS FROM outputs WHERE address = X'$oaddr' ORDER BY created_slot DESC LIMIT $PAGE"
om=$(sqlite3 "$SIEVE_DB" "SELECT count(*) FROM outputs WHERE address = X'$oaddr'")
header "All UTxOs ever at one address — spent included, newest first (full history)." \
       "Deliberate cold path: 'outputs' is PK-only (no secondary index), so expect a SCAN and a loss." \
       "GET /matches/{address}   (no ?unspent → full history)" "$oq" "$om"
measure "" "" "$oq"

log ""
log "done. Every kupo /matches query shape is now measured. The only pattern left"
log "out is the metadata tag, which kupo makes index-only (not a queryable shape)."
