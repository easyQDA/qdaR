#!/bin/sh
# OpenTimestamps proofs for this repository's commits.
#
# For every commit reachable from HEAD, this stamps a small file that holds
# the commit hash and stores an OpenTimestamps proof beside it under
# .timestamps/. A commit hash cryptographically commits the whole snapshot
# (tree + parents), so a tamper-evident, Bitcoin-anchored proof that the hash
# existed by time T is a proof that the commit — and its entire content and
# history — existed by T. Free and decentralised (https://opentimestamps.org).
#
# Run manually with `sh .timestamps/ots-stamp.sh`, or let the post-commit hook
# run it. Point OTS_BIN at the ots client if it is not in ~/.venvs/ots.
set -eu

OTS="${OTS_BIN:-$HOME/.venvs/ots/bin/ots}"
command -v "$OTS" >/dev/null 2>&1 || { echo "ots-stamp: ots not found at $OTS (set OTS_BIN)" >&2; exit 1; }
ROOT="$(git rev-parse --show-toplevel)"
DIR="$ROOT/.timestamps"
mkdir -p "$DIR"

# 1. stamp every commit that has no proof yet
made=0
for sha in $(git -C "$ROOT" rev-list HEAD); do
  f="$DIR/$sha.txt"
  [ -f "$f.ots" ] && continue
  printf '%s\n' "$sha" > "$f"
  if "$OTS" stamp "$f" >/dev/null 2>&1; then
    made=$((made + 1))
  else
    echo "ots-stamp: could not stamp $sha (offline?), will retry next run" >&2
    rm -f "$f"
  fi
done

# 2. upgrade pending proofs once Bitcoin has confirmed them (safe to re-run)
for ots in "$DIR"/*.txt.ots; do
  [ -e "$ots" ] || continue
  "$OTS" upgrade "$ots" >/dev/null 2>&1 || true
done
rm -f "$DIR"/*.ots.bak 2>/dev/null || true

echo "ots-stamp: $made new proof(s); $(ls "$DIR"/*.txt.ots 2>/dev/null | wc -l | tr -d ' ') total in .timestamps/"
