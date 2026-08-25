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

## Refresh / finalise

A fresh proof is *pending* until the next Bitcoin block confirms it (minutes to
a few hours). Afterwards, complete it and commit the updated `.ots`:

```sh
sh .timestamps/ots-stamp.sh   # stamps new commits + upgrades pending proofs
git add .timestamps && git commit -m "timestamps: stamp new commits"
```

New commits are stamped automatically by the local `post-commit` hook (in
`.git/hooks/`, not shared); its proof is committed with the following commit.
Set `OTS_BIN` if the `ots` client is not at `~/.venvs/ots/bin/ots`.
