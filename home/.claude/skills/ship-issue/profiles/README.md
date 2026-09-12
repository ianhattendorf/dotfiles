# Repo profiles

One file per repo, `<repo>.md`. Each defines: **gates**, **invariants source**, **live
verification method**, **merge convention** (and what checkpoint commits become),
**changelog convention**, **never-skip list** (human live run), **extra PR checks**.

A profile carries workflow facts only. Mechanics the repo's own
`CLAUDE.md`/`AGENTS.md` already document are referenced, not restated — those files
load in the same session. A fact marked unverified gets verified in-repo on first use,
and the profile updated.

## Deriving a profile for a new repo

Write it here so every future task inherits it, marking anything unverified:

1. Read `CLAUDE.md`/`AGENTS.md`/`CONTRIBUTING.md`.
2. Gates from `package.json` scripts or Makefile targets (typecheck/lint/test/format).
3. Merge + subject convention by sampling ~20 `main` subjects
   (`gh api repos/gitkraken/<repo>/commits`) — squash shows one commit per PR with
   `(#NNNN)`.
4. Changelog convention: per-version files, a single `CHANGELOG.md`, or none.
5. Live verification from repo docs or the `run` skill.
