# OpenTimestamps proofs

This folder holds independent, tamper-evident proof of **when each commit of
this repository existed** — free, decentralised, and anchored in the Bitcoin
blockchain (<https://opentimestamps.org>).

For a commit `<sha>`:

- `<sha>.txt` contains that commit hash.
- `<sha>.txt.ots` is an OpenTimestamps proof that `<sha>.txt` existed by a
  point in time recorded on the Bitcoin blockchain.

Because a Git commit hash cryptographically commits the entire snapshot (its
tree and all ancestors), proving the hash existed by time *T* proves the whole
commit — content and history — existed by *T*. It proves **existence by a
time**, not authorship and not that nothing existed earlier.

## Verify a proof

```sh
ots verify .timestamps/<sha>.txt.ots      # checks the Bitcoin-anchored time
git cat-file commit <sha> | git hash-object -t commit --stdin   # prints <sha>: ties it to this repo
```

Until the next Bitcoin block confirms a fresh proof, `ots verify` reports
"Pending confirmation in Bitcoin blockchain" — that is expected; run
`ots upgrade` later (see below) and verify again.

## Refresh / finalise — automatic

A fresh proof is *pending* until the next Bitcoin block confirms it (minutes to
a few hours). You do **not** need to finalise it by hand. The local
`post-commit` hook runs `.timestamps/ots-stamp.sh` after every commit, which

1. stamps each new commit,
2. **upgrades** any pending proofs that Bitcoin has since confirmed, and
3. commits the new and upgraded proofs as a `timestamps:` commit.

So the finalised, Bitcoin-anchored proofs accrue on their own as you keep
committing — just `git push` as usual. (One `timestamps:` commit always trails
without its own proof; it is stamped by the next commit.)

Run it once by hand any time — e.g. now, or from a fresh clone:

```sh
sh .timestamps/ots-stamp.sh
```

Set `OTS_BIN` if the `ots` client is not at `~/.venvs/ots/bin/ots`. The hook
lives in `.git/hooks/` and is not shared; re-add it in a fresh clone with:

```sh
printf '#!/bin/sh\nexec sh "$(git rev-parse --show-toplevel)/.timestamps/ots-stamp.sh"\n' \
  > .git/hooks/post-commit && chmod +x .git/hooks/post-commit
```
