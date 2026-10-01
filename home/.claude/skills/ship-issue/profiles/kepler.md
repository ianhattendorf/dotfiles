# ship-issue profile: kepler

Mechanics — script gotchas, `serve:isolate`, the pairing URL, Storybook — are in the
repo `CLAUDE.md`, which loads in every kepler session. This file carries only what the
workflow adds.

- **Gates**: `pnpm check` (oxlint with type-aware rules AND TypeScript compiler diagnostics —
  this REPLACED the old `pnpm typecheck` + `pnpm lint`, which no longer exist as scripts;
  bare `oxlint` misses both, including `no-floating-promises`); `pnpm format:check`;
  `pnpm design:tokens` + `pnpm design:padding` (CI enforces only these two — `pnpm design`
  also runs `design:contrast`, a local-only gate; run all three after a UI edit anyway);
  `pnpm exec vitest run <the tests covering the change>`; `pnpm storybook:smoke` if a
  component's props, mount behavior, or a `.stories.tsx` changed or was deleted (CI doesn't
  gate on it, and nothing else catches an empty render or a mount-time throw).
  What CI actually runs in one job is
  `pnpm check && pnpm format:check && pnpm design:tokens && pnpm design:padding && pnpm exec vitest run`,
  plus the suite as four `--shard=N/4` jobs.
  Only the vitest gate accepts a scope, so it is the only one that can silently narrow.
  **Derive the covering set by grepping the changed identifiers** (a constant name, an
  entitlement id, an exported symbol) across all of `src/`, not from the file list a
  plan or earlier round named — a test that pins a changed value can live anywhere —
  and re-derive after each round. **Before step 10, run the literal `pnpm test` at
  least once**: it is what CI gates on. A new suite that SPAWNS a process also needs a
  `WEIGHTS_SECONDS` entry in `scripts/vitest-sequencer.mts`, or it silently unbalances a shard.
- **Design canvas** (`rules/design-canvas.md`): the brief points at `DESIGN.md` and the
  tokens in `src/ui/index.css`, and names the primitives the surface already uses. Kepler
  ACP sessions get `/design` only on a build carrying the artifact opt-in on the bridge
  spawn; on an older build use that rule's unavailable path.
- **Invariants source**: the enforced-rules list in the repo `CLAUDE.md` (currently
  under "Architecture"; it carries one line + its residual test + a pointer per rule)
  plus the section of `docs/ARCHITECTURE.md` each one points at. Match on the content,
  not the heading text — the heading has been renamed once already.
- **Launching an instance: strip the session-scoped `CLAUDE_CODE_*` vars first.** A
  `pnpm serve:isolate` started from inside a Claude Code session inherits
  `CLAUDE_CODE_CHILD_SESSION=1`, which runs every terminal CLI it spawns with
  **transcript saving off** — no transcript, so no resume handle, so model/mode changes
  are refused and session labels never resolve. It presents as a product bug and has
  burned live runs on kepler#2741 and #2848. Launch with:

  ```
  env -u CLAUDE_CODE_CHILD_SESSION -u CLAUDE_CODE_SESSION_ID -u CLAUDE_CODE_SESSION_ATTENDED \
      -u CLAUDE_CODE_MESSAGING_SOCKET -u CLAUDE_CODE_MESSAGING_TOKEN -u CLAUDE_CODE_ENTRYPOINT \
      -u CLAUDE_CODE_EMIT_SESSION_STATE_EVENTS CLAUDE_CODE_FORCE_SESSION_PERSISTENCE=1 \
      KEPLER_INSTANCE=<slot> pnpm serve:isolate -- --json-ready
  ```

  **Verify before trusting the run, and before handing the URL to the human** —
  `tr '\0' '\n' < /proc/<pid>/environ | grep CLAUDE_CODE_CHILD_SESSION` must be empty
  (pid from `/tmp/kepler-serve-<slot>/*.pid`). A banner reading "transcript saving is
  off — inherited CLAUDE_CODE_CHILD_SESSION marker" means this was skipped.
- **Live verification**: `/live-exercise findings-only` against an instance launched
  that way in the task worktree; stop it with `pnpm serve:isolate stop`
  — NEVER by process-name match, which also kills the real `kepler-server` daemon.
  **Always `findings-only` from a workflow**: unasked it dispatches its own fix agents,
  which `/ship-issue` and `/address-feedback` own themselves and `/investigate` and
  `/review-change` forbid outright. Plain `pnpm serve` only when a test specifically
  needs the shared data dir's live state. Desktop-specific behavior (packaging, PTY,
  window management): the `drive-electron-mcp` skill, which needs a display (Xvfb on a
  headless VM, provisioned at the VM level).
- **Human live run handoff**: launch as above (env stripped, `/proc` checked), then print
  the **owner-token URL** built from its `KEPLER_READY` line — `pairingUrl`
  host and port with `?token=<token>`; `/live-inspect` has the one-line `jq`. Never the
  printed `Remote access URL`: its bootstrap token is single-use and short-lived. The
  owner token is reusable, so one URL serves the agent's sweep and the human's run; the
  human opens it in their own browser. `/live-pair` for interactive iteration. Only
  desktop-shell behavior (packaging, tray, OpenPGP signing, clipboard) needs the fallback:
  human runs `ssh -X agent-vm` + `pnpm dev`, unisolated. **Protocol handlers
  (`kepler-<channel>://` deep links) are the exception, on the VM and on local Linux
  alike:** an unpackaged Linux `electron .` has no `.desktop` file, so no x-scheme-handler
  is registered and a browser's redirect goes nowhere (`xdg-mime query default
  x-scheme-handler/kepler-source` prints nothing). Deliver the URL by hand as a second
  instance: `./node_modules/.bin/electron . '<scheme>://auth/callback?code=…'` from the
  worktree, with the dev app running (add `--user-data-dir=<dir>` for a `dev:isolate`
  profile) — the single-instance lock forwards it like the OS would (verified
  2026-09-30, kepler#3228). A remote-bound window runs its daemon's renderer, so testing a
  renderer change there needs the daemon on this build: `rm -rf out/ui out/renderer &&
  pnpm create:remote:linux:x64`, then reconnect (kepler#3279: the bundle otherwise packs a
  stale `out/ui`). Never remove
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
- **Security-sensitive surfaces** (route the diff through `security-review`, per
  SKILL.md step 5): auth tokens and account credentials, the HTTP/WebSocket transport
  boundary and `HTTP_MAP` policy, packaging identity and Electron fuses, the renderer
  clipboard, MCP tool identity and auto-approval, and any change to the lockfile or a
  direct dependency.
- **Extra PR checks**: telemetry changes mirror `TELEMETRY.md` in the same change —
  reviewers bounce the PR on it.
