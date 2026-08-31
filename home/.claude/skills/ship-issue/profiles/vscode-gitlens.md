# ship-issue profile: vscode-gitlens

Seed profile (2026-08-28), partially verified. The repo's own `CLAUDE.md`,
`AGENTS.md`, and `CONTRIBUTING.md` are authoritative over this file — read them on
first use and correct/extend this profile.

- **Gates**: package.json scripts include `check`, `lint:ci`, `fmt:check`, `test`,
  `build:quick` — confirm the expected pre-PR set against `CONTRIBUTING.md`/CI config.
- **Invariants source**: repo `CLAUDE.md` + `AGENTS.md`.
- **Live verification**: the extension host / `web:serve`; no live-exercise skill —
  use the repo's launch method (the generic `run` skill can drive it).
- **Merge convention**: **not squash-style** (verified against `main`, 2026-08-28) —
  `main` carries per-commit, sentence-style imperative subjects ("Gates the lazy
  transcript read on the agent's capability"), mostly without PR numbers. Assume
  intra-branch commits land on `main`: before the first push, squash/reword
  checkpoints locally into commits matching that subject style.
- **Changelog**: single `CHANGELOG.md`, updated **in the change itself** (unreleased
  section) — the changelog edit is part of the PR.
- **Never-skip list (human live run)**: not yet established — build it from first
  use and record it here.
- **Extra PR checks**: not yet established (note: telemetry docs are generated —
  `generate:docs:telemetry` — check whether telemetry changes require regeneration).
