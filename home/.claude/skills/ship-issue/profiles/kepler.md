# ship-issue profile: kepler

Mechanics — script gotchas, `serve:isolate`, the pairing URL, Storybook — are in the
repo `CLAUDE.md`, which loads in every kepler session. This file carries only what the
workflow adds.

- **Gates**: `pnpm typecheck`; `pnpm lint` (a `const { x: _x, ...rest } = y` strip needs
  `// oxlint-disable-next-line typescript/no-unused-vars` above it — the config ignores
  underscore-prefixed args only, not destructured vars); `pnpm exec vitest run <the
  tests covering the change>`; `pnpm design` if any UI was touched; `pnpm format:check`;
  `pnpm storybook:smoke` if a component's props, mount behavior, or a `.stories.tsx`
  changed or was deleted (CI doesn't gate on it, and nothing else catches an empty
  render or a mount-time throw).
  Only the vitest gate accepts a scope, so it is the only one that can silently narrow.
  **Derive the covering set by grepping the changed identifiers** (a constant name, an
  entitlement id, an exported symbol) across all of `src/`, not from the file list a
  plan or earlier round named — a test that pins a changed value can live anywhere —
  and re-derive after each round. **Before step 10, run the literal `pnpm test` at
  least once**: it is what CI gates on.
- **Invariants source**: repo `CLAUDE.md` "Rules that aren't obvious from the code" +
  `docs/ARCHITECTURE.md`.
- **Live verification**: `/live-exercise` against an instance launched with
  `pnpm serve:isolate` in the task worktree. Plain `pnpm serve` only when a test
  specifically needs the shared data dir's live state. Desktop-specific behavior
  (packaging, PTY, window management): the `drive-electron-mcp` skill, which needs a
  display (Xvfb on a headless VM, provisioned at the VM level).
- **A broken live-verification tool is blocking, not a silent skip.** Stop and tell the
  human what you tried; do not substitute weaker verification (unit tests, a single
  human run) and report the step as done — a degraded step looks identical to a passed
  one in the final report.
- **Event-driven UI code needs its async interleavings reasoned through explicitly.**
  Two independent sources correlated only by an id (an RPC response and a broadcast
  event; a streamed chunk and an exit signal) can arrive in either order, and a
  listener gated on a not-yet-known id can race the event meant to fill it. A static
  diff read misses this because the code looks correct in sequence; only injecting
  out-of-order delivery against a mounted runtime reveals it. When a change adds a new
  pair of independent async sources, walk every interleaving in review, and name any
  gap as an UNVERIFIED risk in the PR body rather than a generic "couldn't test the UI".
- **Human live run handoff**: `pnpm serve:isolate -- --json-ready` in the task worktree,
  then print the **owner-token URL** built from its `KEPLER_READY` line — `pairingUrl`
  host and port with `?token=<token>`; `/live-inspect` has the one-line `jq`. Never the
  printed `Remote access URL`: its bootstrap token is single-use and short-lived. The
  owner token is reusable, so one URL serves the agent's sweep and the human's run; the
  human opens it in their own browser. `/live-pair` for interactive iteration. Only
  desktop-shell behavior (packaging, tray/protocol handlers, OpenPGP signing, clipboard)
  needs the fallback: human runs `ssh -X agent-vm` + `pnpm dev`, unisolated. Never remove
  a live PID stamp in any data dir, isolated or shared; if a non-isolated instance is
  already running and this task needs the shared dir specifically, coordinate with the
  human. (`dev:remote` develops Kepler's remote-window feature; it is not a
  remote-testing path.)
- **Merge convention**: **squash** — only the PR title lands on `main`, in conventional
  `type(scope): subject` format, framed by user impact (it is what changelog mining
  reads). Intra-branch history is working state: no fixup/rebase ceremony; checkpoint
  subjects plain and honest. Same per phase PR at feature scale.
- **Changelog**: mined at release from the version window (`generate-changelog` skill) —
  never write per-PR entries. If phase PRs may straddle a release: a surface-less PR's
  body says "no user-facing changes" and names the PR that ships the surface; the final
  PR's body carries the user-facing summary — the single entry belongs to the release
  where the feature becomes user-visible (flag graduation counts if the UI ships
  default-off).
- **Never-skip list (human live run)**: UI changes, event-flow/multi-window changes,
  anything touching the terminal or agent-session paths.
- **Extra PR checks**: telemetry changes mirror `TELEMETRY.md` in the same change —
  reviewers bounce the PR on it.
