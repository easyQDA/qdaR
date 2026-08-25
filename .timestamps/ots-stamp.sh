#!/bin/sh
# OpenTimestamps proofs for this repository's commits.
#
# On every run it (1) stamps each commit reachable from HEAD that has no proof
# yet — a Bitcoin-anchored proof that the commit existed by the stamp time —,
# (2) UPGRADES pending proofs once Bitcoin has confirmed them, and (3) commits
# the new and upgraded proofs as a "timestamps:" maintenance commit, so the
# finalised proofs land in the repository without any manual step. A commit
# hash commits the whole snapshot, so proving the hash existed by time T proves
# the commit — content and history — existed by T. Free and decentralised
# (https://opentimestamps.org).
#
# The local post-commit hook runs this after each commit, so upgrades happen
# regularly on their own. Run by hand as `sh .timestamps/ots-stamp.sh`.
# Set OTS_BIN if the ots client is not at ~/.venvs/ots/bin/ots.
set -eu

OTS="${OTS_BIN:-$HOME/.venvs/ots/bin/ots}"
command -v "$OTS" >/dev/null 2>&1 || { echo "ots-stamp: ots not found at $OTS (set OTS_BIN)" >&2; exit 0; }
ROOT="$(git rev-parse --show-toplevel)"
DIR="$ROOT/.timestamps"
mkdir -p "$DIR"

# Re-entrant run: when our own maintenance commit fires the post-commit hook,
# OTS_STAMP_NO_COMMIT is set. Then we ONLY upgrade — no stamping, no commit —
# which prevents recursion AND keeps the working tree clean.
maint="${OTS_STAMP_NO_COMMIT:-}"

# 1. stamp every commit without a proof yet
if [ -z "$maint" ]; then
  for sha in $(git -C "$ROOT" rev-list HEAD); do
    f="$DIR/$sha.txt"
    [ -f "$f.ots" ] && continue
    printf '%s\n' "$sha" > "$f"
    "$OTS" stamp "$f" >/dev/null 2>&1 || { echo "ots-stamp: could not stamp $sha (offline?)" >&2; rm -f "$f"; }
  done
fi

# 2. upgrade pending proofs to their Bitcoin attestation (safe to re-run)
for ots in "$DIR"/*.txt.ots; do
  [ -e "$ots" ] || continue
  "$OTS" upgrade "$ots" >/dev/null 2>&1 || true
done
rm -f "$DIR"/*.ots.bak 2>/dev/null || true

# 3. persist new + upgraded proofs as one commit, unless this is the re-entrant
#    run or a rebase/merge/cherry-pick is in progress (never commit into those).
if [ -z "$maint" ]; then
  gd="$(git -C "$ROOT" rev-parse --git-dir)"
  busy=""
  for m in rebase-merge rebase-apply MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD; do
    [ -e "$gd/$m" ] && busy=1
  done
  if [ -z "$busy" ] && [ -n "$(git -C "$ROOT" status --porcelain -- .timestamps)" ]; then
    git -C "$ROOT" add .timestamps
    OTS_STAMP_NO_COMMIT=1 git -C "$ROOT" \
      -c user.name="fre.ms" -c user.email="fre.ms@fre.ms" \
      commit -q -m "timestamps: stamp new commits and upgrade proofs" || true
  fi
fi

echo "ots-stamp: $(ls "$DIR"/*.txt.ots 2>/dev/null | wc -l | tr -d ' ') proof(s) in .timestamps/"
