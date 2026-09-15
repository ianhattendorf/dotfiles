# Repo profiles

One file per repo, `<repo>.md`. Each defines: **gates**, **invariants source**, **live
verification method**, **merge convention** (and what checkpoint commits become),
**changelog convention**, **never-skip list** (human live run),
**security-sensitive surfaces**, **extra PR checks**.

A profile carries workflow facts only. Mechanics the repo's own
`CLAUDE.md`/`AGENTS.md` already document are referenced, not restated — those files
load in the same session. A fact marked unverified gets verified in-repo on first use,
and the profile updated.

**Any edit to a profile re-verifies every field, not just the field being changed.** A
repo change that moves one fact usually moves its neighbours: the pointer that broke
this rule into existence survived a gates update made in the same window by the same
commit that invalidated it. Re-read the repo's own docs end to end, cheaply, before
saving.

**Prefer content descriptors to quoted headings** when pointing into a repo doc. A
heading string is the most brittle thing to depend on and gives no signal when it
drifts — a grep just returns nothing, and the step consuming it reports no invariants
rather than an error.

## The dependency runs one way

A repo file must never depend on these skills for a step to happen. Profiles and the
ship-issue rules read the repo — its `CLAUDE.md`, `docs/`, and any skills it commits
under `.claude/skills/`; anything a repo step REQUIRES resolves inside the repo.

This matters because a repo's own skills are read by harnesses that have no access to
this directory (Codex, Cursor, agy via mirrored skill dirs) and by colleagues who do
not have it at all. A committed file saying "run the profile's gates" or "see
`/ship-issue`" resolves to nothing for them, and it fails silently — the step is simply
not performed, which is indistinguishable from a step that passed.

**A guarded optional reference is a different thing and is fine.** "`/review` /
`/deep-review` — static counterparts, if available" names skills that some readers have
and others do not, and the "if available" is what makes it safe: a reader who lacks them
checks, finds nothing, and proceeds, having lost nothing the repo guaranteed. Kepler's
live-* skills carry several of these, deliberately, so each dev's own global review
skill gets picked up. Do not strip them — the test is not "does this name something
outside the repo" but **"does a reader without it silently skip a step that mattered".**
Required and unguarded is the defect; optional and guarded is a courtesy to whoever has
the tool.

When a repo skill needs behavior that only an outer workflow can supply, it exposes a
**mode**, named without reference to the workflow ("when an outer workflow owns the
commit boundary…"), and the profile selects that mode. Never the reverse.

## Deriving a profile for a new repo

Write it here so every future task inherits it, marking anything unverified:

1. Read `CLAUDE.md`/`AGENTS.md`/`CONTRIBUTING.md`.
2. Gates from `package.json` scripts or Makefile targets (typecheck/lint/test/format).
3. Merge + subject convention by sampling ~20 `main` subjects
   (`gh api repos/gitkraken/<repo>/commits`) — squash shows one commit per PR with
   `(#NNNN)`.
4. Changelog convention: per-version files, a single `CHANGELOG.md`, or none.
5. Live verification from repo docs or the `run` skill.
