# Second-model review — Codex as an independent reader

A fresh Claude subagent removes the writing session's context but not the model
family's blind spots. Codex is a reader from a different family that reads the same repo
rules (Kepler's `AGENTS.md` is a symlink to `CLAUDE.md`; `codex exec review`'s built-in
prompt reads and cites the applicable instruction file per finding). It is **read-only
and advisory**: it never edits, never runs inside inner review rounds, never implements,
and never decides. What it returns enters the calling step's triage like any other
reviewer's findings or advocate's case.

## When it runs

- The **last correctness round before a push** (`fix-or-reject.md`), beside the Claude
  subagent — review form, effort `high`.
- The **oscillation escalation** in ship-issue step 5, beside the `xhigh` Claude
  subagent — review form, effort `xhigh`.
- `/review-change` step 4 — review form, effort `high`; without the intent line when no
  spec exists.
- **Every plan before the human approves it** (ship-issue step 2, and a feature-scale
  `PLAN.md`) — plan-review form, effort `high`.
- **`/approach` option generation**, blind, beside Claude's own enumeration — option
  form, effort `high`.
- The **qualitative comment audit** (`comment-audit.md`) may be dispatched to it —
  comment-audit form, effort `high`.

A run that fails to start or exceeds ~15 minutes is a **step that could not run**:
blocking under ship-issue's never-skip rule, never a silent skip.

**The sandbox stays read-only.** That was not what limited it on kepler#3127: its runs
there only read files and diffs, and never tried a command that the sandbox refused.
Its false finding in the later `/review-change` came from a wrong code comment, and
that run came from an interactive Codex session that could run tests (237 passed).
Running tests would not have caught it. What it needs is the primary sources
(`fix-or-reject.md`, "What the dispatch prompt carries"). The Claude side proves a
Codex suspicion empirically during triage, with a scratch repro in its own scratchpad.
A writable sandbox is `workspace-write`, which can edit the very worktree under review.

## Model

Every form passes `-m` with the slug `codex-model.py` (beside this file) prints. The
pin is Codex's frontier tier because the Claude side runs the frontier tier too; the
unpinned default is the workhorse tier (`gpt-6-sol` as of codex-cli 0.157.0). The
script is the only place the slug is written. It checks the pin against
`~/.codex/models_cache.json`, which the CLI refreshes on every run:

- **Exit 0**: stdout is the slug. Dispatch.
- **Exit 2, stale**: the pin is no longer listed, or Codex carries an upgrade path for
  it (the retirement notice is printed). Blocking. Ask the human with `AskUserQuestion`
  which model replaces it, the upgrade target first and "(Recommended)".
- **Exit 3, new**: Codex lists a model that was not listed when the pin was chosen.
  Ask the human with `AskUserQuestion` whether to switch, "Keep <pin>" and "Switch to
  <new>" with each one's listed description.

After either question, edit the script: `PIN` to the chosen slug and `KNOWN` to the
set it just printed, so the same release is not asked about twice. Then dispatch. A
missing cache prints a warning and exits 0; the run itself then fails loudly on a bad
slug.

## Invocation

Resolve the target's diff before dispatching — it is an input to the prompt, not
something the template derives. For ship-issue's own branch that is
`merge-base(origin/main, HEAD)...HEAD`; for a merged PR it is the merge commit from the
PR detail (`gh pr view N --json mergeCommit,commits`: a squash is that one commit, a
rebase-merge is the last `commits.length` commits ending there, a true merge is
`sha^1..sha`), never the checked-out branch; a commit is `sha^..sha`; a range is as given.

```
model=$(python3 ~/.claude/skills/ship-issue/rules/codex-model.py) || exit $?
out=$(mktemp -t codex-review-XXXXXX.md)
codex exec review --ephemeral -m "$model" -c sandbox_mode='"read-only"' \
  -c model_reasoning_effort='"high"' -o "$out" "$(cat <<EOF
Review the change \`git diff <resolved diff spec>\`.
Intent and acceptance criteria: <from the issue / task note, verbatim; omit this line for
a cold read>.
Report only findings with a concrete failure scenario: a specific input or state
leading to a specific wrong output. Do not report style, formatting, or pre-existing
issues.
For every new trigger into existing shared or async state (a cache, lock, in-flight
guard, event subscription), reason about what happens if it fires while another trigger
on the same state is mid-flight. For any new pair of independent sources correlated
only by an id, walk each arrival order.
EOF
)" < /dev/null
```

Facts the flags encode — each was checked against codex-cli 0.155.1, `-m` against
0.157.0:

- **The prompt is three parts and nothing else**: diff range, intent, the interleaving
  directive lifted from `fix-or-reject.md`. Do not restate repo invariants or the
  generic finding bar — the built-in system prompt already reads the instruction file,
  cites it per finding, excludes style and pre-existing bugs, and tags findings
  `[P0]`–`[P3]`. Restating pushes it toward generic findings.
- **`--base` and `--commit` are never used.** Each is mutually exclusive with a prompt,
  neither can express a range, and a prompt without the intent line already gets the
  built-in review guidelines — which is all a cold read needs. The one exception is a
  dirty working tree, where `--uncommitted` (no prompt) is the honest form.
- **Effort is pinned because the review subcommand's default is `none`**, not the
  model's `medium`. `xhigh` at the oscillation escalation. The inner quotes are
  required: the value is parsed as TOML. Do not set this or the model in `config.toml`
  — it would change every interactive Codex session too.
- **`< /dev/null` is required**: `codex exec` reads stdin whenever it is not a TTY,
  even with a prompt argument, so a backgrounded run with stdin left open prints
  "Reading additional input from stdin..." and hangs forever (seen 2026-09-28, kepler#3228).
- **`--ephemeral`** writes no rollout under `~/.codex/sessions`. Kepler discovers
  external Codex sessions by walking that directory, so a non-ephemeral run would appear
  in the task's session list as a resumable conversation. `--thread-source` labels a
  persisted thread only; with `--ephemeral` there is nothing to label — do not add it.
- **`-o` holds the last message as plain text**: prose on an empty review, tagged
  findings otherwise. Read it; never parse it as JSON. `mktemp` because two reviews can
  overlap (a round still running while the next is dispatched, or two tasks in one
  machine).
- **`sandbox_mode` is pinned because `review` has no `-s` flag and a trusted project
  (every GK repo is, per `~/.codex/config.toml`) defaults it to `workspace-write`**,
  which lets the reviewer edit the worktree. Read-only is what makes it advisory.

### The other forms

Plain `codex exec`, not `review`: the review subcommand's built-in prompt is written for
a diff. Same `-m`, `--ephemeral`, `sandbox_mode`, effort, `-o` and `< /dev/null`, run from the
repo worktree so the instruction file loads. Dispatch it in the background and carry on
with the Claude side; the calling step waits for it before its triage.

**Comment-audit form**: given `comment-audit.md`'s "The bar" section, the diff range,
and the instruction to return per-comment keep / delete / trim verdicts with a one-line
reason and edit nothing.

**Plan-review form**: write the plan to a file first (plan mode's plan file, or a copy
in the session scratchpad; `PLAN.md` at feature scale). The prompt carries:

```
Read the implementation plan at <path>. It is not yet approved or implemented.
Intent and acceptance criteria: <verbatim>.
Settled approach: <the approach note's Decision and Options considered, verbatim; or
"none recorded">. Do not re-propose a rejected option.
Invariant sources beyond the instruction file: <the profile's invariants-source paths>.
Check the plan against the repository as it stands. Report only concrete gaps: an
invariant the plan would break; a file, call site, or existing consumer of a changed
type, event or enum member that must change but is not named; a planned test that would
pass without the fix; an ordering or interleaving hazard the plan does not handle. For
each, give the plan line, the repository evidence as file:line, and the specific failure
it leads to. If a gap can only be closed by a different approach, say so in one line
instead of designing the alternative. No style, wording, or alternative plans.
```

**Option form**: given only `/approach` step 1's frame, never Claude's options or its
leaning — an option list it has seen anchors it:

```
Problem: <verbatim from the verification note, issue or explore note>.
Acceptance criteria: <verbatim>.
Invariant sources beyond the instruction file: <the profile's invariants-source paths>.
Propose the two or three most credible ways to build this, at altitude: name seams,
never files, test files or phases. For each give its shape in one paragraph; the
invariants it touches and any new concept it introduces (an event, a table or
migration, a setting, a flag, a wire route); what it forecloses; its size and main
risk; and the unknown that would flip the choice. Make each option's strongest case
including its costs, and do not recommend one. If only one approach is credible, say so
and give the reason each alternative you considered fails.
```

The option fields mirror `/approach`'s card shape; change them together.

## Triage

- **Review and comment-audit output** goes through `fix-or-reject.md` unchanged. A
  Codex-only finding gets the same concrete-scenario bar, not deference; a finding both
  reviewers raise is near-certain and is recorded as such.
- **Plan-review findings** meet the same bar, applied to a plan: a named gap with repo
  evidence, checked against the repo before it changes anything. An accepted finding
  revises the plan; a rejected one gets a one-line reason. The plan put to the human
  carries a short "Second-model plan review" list of both, because the human is the one
  approving it and a rejected finding is theirs to overrule. A gap only a different
  approach can close goes back to `/approach` under step 2's rule, not into a revision.
- **Codex options** join `/approach`'s cards on equal terms. One materially distinct
  from every Claude option becomes its own card, cut to the card limit; it needs no
  `Plan`-subagent advocate, since the prompt already asked for its strongest case. One
  that matches a Claude option merges into that card, with any cost only one side named.
  A credible Codex option also closes the single-credible-option exit.

## Verified behavior

2026-09-22, kepler commit b30b60e01, 374 lines: both review forms cite the repo
instruction file per finding without being told to (`AGENTS.md#L84` from a run without
intent, `CLAUDE.md:90` from one with it), so the prompt needs no pointer to it. Two
concurrent runs wrote distinct `mktemp` files and left the rollout count unchanged.

2026-09-24, kepler#3118: an ephemeral run still appears in Kepler as a terminated
"Codex (external)" session, because the user-level hooks fire regardless of
`--ephemeral`. That is expected and fine: do not add `--ignore-user-config` to suppress
it, since review runs must load the user's Codex config.
