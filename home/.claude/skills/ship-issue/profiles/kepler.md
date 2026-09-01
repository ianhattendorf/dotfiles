# ship-issue profile: kepler

- **Gates**: `pnpm typecheck`; `pnpm lint` (`oxlint --type-aware --type-check`;
  `.oxlintrc.json`'s `no-unused-vars` only ignores underscore-prefixed function
  args, not destructured vars — a `const { x: _x, ...rest } = y` strip needs
  `// oxlint-disable-next-line typescript/no-unused-vars` above it, precedented
  in `adapter-spawn.ts`); `pnpm exec vitest run <the tests covering the change>`
  (existing tests over edited code, not just test files you edited — NEVER
  `pnpm test -- <file>`; the filter is dropped and the whole suite runs);
  `pnpm design` if any UI was touched; `pnpm format:check`.
- **Invariants source**: repo `CLAUDE.md` "Rules that aren't obvious from the code" +
  `docs/ARCHITECTURE.md`.
- **Live verification**: `/live-exercise`, driving an instance launched with
  `pnpm serve:isolate` (kepler#2308, merged in #2312) — the agent's own verification
  contends for the shared data dir exactly like the human run does, and the live-*
  skills operate on URL+token the same way regardless of which script launched the
  instance. Plain `pnpm serve` is the exception, only when a test specifically needs
  the shared data dir's live state. For desktop-specific behavior (packaging, PTY,
  window management) use the `drive-electron-mcp` skill.
- **If `pnpm serve:isolate` is missing in the worktree**, the branch forked from
  `main` before kepler#2312 — that is a freshness-check finding (rebase onto
  `origin/main` per the resume check), NOT a reason to fall back to shared-dir
  serve.
- **Human live run handoff**: run `pnpm serve:isolate` in the task worktree and
  print the **Remote access URL** it emits to chat — the human opens it in their
  local browser (tunnel; no ssh -X needed). `serve:isolate` gives this task worktree
  its own branch-keyed data dir, seeded once from the VM's local data dir so login +
  repos carry over, and runs with `--isolated` so it never touches the shared `gk`
  auth session. This is what makes **concurrent live instances across task
  worktrees on one VM** safe: each isolated instance has its own PID/port/token
  stamps under `$TMPDIR/kepler-serve-<branch>`, so a second task's `serve:isolate`
  cannot collide with this one or with a plain `pnpm serve`/`pnpm dev`. For
  interactive iteration use `/live-pair`. Only desktop-shell behavior (packaging,
  tray/protocol handlers, OpenPGP signing, clipboard) needs the fallback: human runs
  `ssh -X agent-vm` + `pnpm dev` (unisolated, since desktop-shell surfaces aren't
  exercised by serve at all). Coordinate-don't-delete now applies per data dir:
  never remove a live PID stamp, in ANY task's isolated dir or the shared one —
  if a plain (non-isolated) `pnpm serve`/`pnpm dev`/`drive-electron-mcp` instance is
  already running and this task needs the shared dir specifically, report it and
  coordinate with the human instead. (`dev:remote` is unrelated — it develops
  Kepler's remote-window feature, not a remote-testing path.)
- **Merge convention**: **squash** — only the PR title lands on `main`, in
  conventional format (`type(scope): subject`). Intra-branch history is working
  state: no fixup/rebase ceremony; checkpoint subjects are plain and honest; the PR
  title is the commit message that matters (framed by user impact — it is what lands
  on `main` and what changelog mining reads; same per phase PR at feature scale).
- **Changelog**: mined at release time from the version window (`generate-changelog`
  skill) — never write per-PR entries. If phase PRs may straddle a release: a
  surface-less PR's body states "no user-facing changes" and names the PR that ships
  the surface; the final PR's body carries the user-facing summary — the single
  changelog entry belongs to the release where the feature becomes user-visible (flag
  graduation counts, if the UI ships default-off).
- **Never-skip list (human live run)**: UI changes, event-flow/multi-window changes,
  anything touching the terminal or agent-session paths.
- **Extra PR checks**: telemetry changes MUST mirror `TELEMETRY.md` in the same
  change — reviewers will bounce the PR on it.
