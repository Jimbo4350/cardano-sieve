#!/usr/bin/env bash
#
# Launch the preview cardano-node that bench/run-sync-bench.sh replays from.
#
# This only *launches* an already-provisioned node — the node bundle and a
# synced DB must already exist. For first-time setup (download the node bundle
# and bootstrap ~/preview.db from a Mithril snapshot) use ~/sync-preview.sh.
#
# Env vars line up with run-sync-bench.sh so the two agree on socket + config:
#   NODE_SOCKET    (default ~/node.socket)      — node-to-client socket to serve
#   NODE_CONFIG    (default ~/cardano-bin/11.0.1/share/preview/config.json)
#   TESTNET_MAGIC  (default 2 = preview)         — informational here
# Node-specific:
#   NODE_BIN       (default ~/cardano-bin/11.0.1/bin/cardano-node)
#   DB_DIR         (default ~/preview.db)
#   TOPOLOGY       (default <NODE_CONFIG dir>/topology.json)
#   NODE_PORT      (default 3000)
set -euo pipefail

NODE_SOCKET="${NODE_SOCKET:-$HOME/node.socket}"
NODE_CONFIG="${NODE_CONFIG:-$HOME/cardano-bin/11.0.1/share/preview/config.json}"
TESTNET_MAGIC="${TESTNET_MAGIC:-2}"
NODE_BIN="${NODE_BIN:-$HOME/cardano-bin/11.0.1/bin/cardano-node}"
DB_DIR="${DB_DIR:-$HOME/preview.db}"
NODE_PORT="${NODE_PORT:-3000}"

die() { echo "FATAL: $*" >&2; exit 1; }

[ -f "$NODE_CONFIG" ] || die "node config missing at $NODE_CONFIG (set NODE_CONFIG)"
CONFIG_DIR="$(cd "$(dirname "$NODE_CONFIG")" && pwd)"
TOPOLOGY="${TOPOLOGY:-$CONFIG_DIR/topology.json}"

# ---- fail loudly if anything the node needs is missing ----------------------
[ -x "$NODE_BIN" ] || die "cardano-node not found/executable at $NODE_BIN
  set NODE_BIN, or run ~/sync-preview.sh to fetch the node bundle."
[ -f "$TOPOLOGY" ] || die "topology missing at $TOPOLOGY"
[ -d "$DB_DIR/immutable" ] || die "no synced DB at $DB_DIR (no immutable/ dir)
  provision it first with ~/sync-preview.sh, then re-run this."

# ---- don't clobber a node already running on this socket --------------------
if [ -S "$NODE_SOCKET" ]; then
  if pgrep -af "cardano-node.*$NODE_SOCKET" >/dev/null 2>&1; then
    echo "A cardano-node is already running on $NODE_SOCKET:" >&2
    pgrep -af "cardano-node.*$NODE_SOCKET" | sed 's/^/    /' >&2
    die "refusing to start a second one — stop it with: pkill -f \"cardano-node.*$NODE_SOCKET\""
  fi
  echo "==> removing stale socket $NODE_SOCKET"
  rm -f "$NODE_SOCKET"
fi

echo "==> starting cardano-node (preview, testnet-magic $TESTNET_MAGIC)"
echo "    bin:    $NODE_BIN"
echo "    db:     $DB_DIR"
echo "    socket: $NODE_SOCKET"
echo "    config: $NODE_CONFIG"
echo "    watch:  cardano-cli query tip --socket-path $NODE_SOCKET --testnet-magic $TESTNET_MAGIC"
echo ""

# RTS flags mirror ~/sync-preview.sh (modest -N2, memory returned promptly).
exec "$NODE_BIN" run \
  --port "$NODE_PORT" \
  --database-path "$DB_DIR" \
  --topology "$TOPOLOGY" \
  --config "$NODE_CONFIG" \
  --socket-path "$NODE_SOCKET" \
  +RTS -T -I0 -N2 -A16m -qb -qg --disable-delayed-os-memory-return -RTS
