# Second-model review — an independent model family

A fresh same-family reviewer removes the writer's context; a second family provides
an independent read. Both are read-only and advisory. The parent triages findings,
implements fixes, and owns decisions. Read `host-runtime.md` for host mechanics.

## When it runs

- The **last correctness round before a push** (`fix-or-reject.md`), beside the primary
  subagent — review form, effort `high`.
- The **oscillation escalation** in ship-issue step 5, beside the `xhigh` primary
  subagent — review form, effort `xhigh`.
- `/review-change` step 4 — review form, effort `high`; without the intent line when no
  spec exists.
- **Every plan before the human approves it** (ship-issue step 2, and a feature-scale
  `PLAN.md`) — plan-review form, effort `high`.
- **`/approach` option generation**, blind, beside the writer's own enumeration — option
  form, effort `high`.
- The **qualitative comment audit** (`comment-audit.md`) may be dispatched to it —
  comment-audit form, effort `high`.

A run that fails to start or exceeds ~15 minutes is a **step that could not run**:
blocking under ship-issue's never-skip rule, never a silent skip.

Reviewer tools stay read-only. The writer investigates suspicions empirically during
triage; inability to execute a test inside the reviewer is not a reason to grant it
write access. Give it primary sources, not just the writer's paraphrase.

## Model selection — all forms

| Writing host | Second reviewer |
| --- | --- |
| Claude | Codex Astra, `gpt-6-astra` |
| Codex | Claude Opus, explicit `--model opus` |

This applies to diff, plan, option, escalation, and comment-audit forms. Never use
an unpinned default or a different tier as fallback. These flags do not change the
writing session or global configuration.

For Codex, `codex-model.py` beside this file is the single executable pin. Exit 0
prints Astra; exit 2 means the cache explicitly lacks the pin, and dispatch blocks.
Unrelated new models and upgrade suggestions do not change the user's selection.
An unreadable cache warns and attempts the explicit pin; a CLI rejection still
blocks. For Claude, the installed CLI supports the `opus` alias. Capture its actual
model ID from the JSON result's model usage when available. Authentication errors,
model rejection, timeouts, and permission denials needed for the review block the
step. Report them; changing the selected model requires a user decision.

## Prepare the review

Resolve the target before dispatch: branch `merge-base(origin/main, HEAD)...HEAD`,
commit `sha^..sha`, explicit range as given, merged PR's actual merge/rebased range,
or the dirty working tree. Never accidentally review the current branch for an
unrelated target. Materialize dirty/untracked changes when the CLI cannot express
the exact scope. Record the baseline and include all relevant added files.

Resolve the directory containing this rule to an absolute `rules_dir` and the
review brief to an absolute `brief` path. Run from the target repository so its
instructions load. The brief contains the exact scope, intent, acceptance criteria,
primary-source pointers and the form below. Do not include the writer's transcript.
Use distinct scratch files per run; do not put review artifacts in the tracked diff.

Run through the host's background/session execution facility and poll to completion,
with a 15-minute deadline. Record the session or launch PID for cleanup; stop only
that owned run on timeout. Read output only after checking completion and status.
The CLI needs network/auth access and writable runtime state outside the worktree.
If the parent sandbox denies those, request the host's scoped execution escalation
with a reason, keeping the reviewer's read-only/tool restrictions intact. Never
disable the reviewer's restrictions to solve a parent sandbox failure. A rejected
escalation or policy forbidding it leaves the review blocked.

## Claude writer → Codex Astra

```sh
model=$(python3 "$rules_dir/codex-model.py") || exit $?
out=$(mktemp -t codex-review-XXXXXX.md)
codex exec review --ephemeral -m "$model" -c sandbox_mode='"read-only"' \
  -c model_reasoning_effort='"high"' -o "$out" "$(cat "$brief")" < /dev/null
```

For a plan, options, comment audit, or a prepared diff file, use plain `codex exec`
with the same flags. `review` supplies built-in diff-review guidance; do not use it
for a plan. Do not combine `--base` or `--commit` with a prompt. For dirty changes,
prefer a prepared diff and plain exec when intent must accompany the review.

Use `xhigh` for escalation. Explicit read-only is required even for trusted repos.
Closing stdin avoids a background invocation waiting for more input. `-o` is plain
text, not JSON. Ephemeral runs do not persist a resumable rollout, although hooks
may still produce a terminated external-session row in Kepler. Read the output and
check the exit status; absence of findings is not proof the command succeeded.

## Codex writer → Claude Opus

Prepare the diff as a file and name it in the brief: this reviewer has no shell to
run git. Include the repo instruction paths, `fix-or-reject.md`, and any required
plan/source files. Put scratch inputs in one directory and pass it as `inputs_dir`.
Only add other directories when a referenced source lies outside the repo/inputs.

```sh
out=$(mktemp -t claude-review-XXXXXX.json)
claude --print --model opus --effort high --restricted \
  --tools Read,Glob,Grep --allowedTools Read,Glob,Grep \
  --permission-mode dontAsk --strict-mcp-config --mcp-config '{"mcpServers":{}}' \
  --settings '{"disableAllHooks":true}' --no-session-persistence \
  --add-dir "$inputs_dir" "$rules_dir" --output-format json "$(cat "$brief")" < /dev/null > "$out"
```

This allowlist excludes shell, edits, writes, agent spawning and MCP mutations.
Restricted mode ignores ordinary settings/plugins, confines file tools to working
and explicitly added directories, and hooks are disabled for this review process.
Do not add a fallback model or bypass-permissions flag. If these flags are not
supported by the installed CLI, the review blocks until a supported read-only
runner is available; do not drop the restrictions to get it running.

Check exit status, `is_error`, `permission_denials`, and `result`; retain
`modelUsage` as evidence of the selected model. A denial of a source needed for the
review is a blocked step, not a clean finding set. Use xhigh for escalation. For
all other forms, change the brief, not the model or tool restrictions.

The same restricted CLI procedures can supply a primary reviewer when the native
fresh-agent tool cannot enforce read-only access. In that case use the primary
review brief and the same family as the writer; the independent second review
still runs in the other family.

## Briefs

**Diff form**: name the resolved range or prepared diff file and original intent.
For Claude/plain exec, also request the repo's applicable instruction files and
`fix-or-reject.md`. Report only introduced concrete failure scenarios, with
file/line, specific input/state, wrong output, and severity; exclude style and
pre-existing defects. For each new trigger into shared async state, reason through
interleavings; for independent sources correlated by ID, walk each arrival order.

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

**Option form**: given only `/approach` step 1's frame, never the writer's options or its
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
  second-reviewer-only finding gets the same concrete-scenario bar, not deference; agreement is supporting evidence, not proof; verify accepted findings against primary sources.
- **Plan-review findings** meet the same bar, applied to a plan: a named gap with repo
  evidence, checked against the repo before it changes anything. An accepted finding
  revises the plan; a rejected one gets a one-line reason. The plan put to the human
  carries a short "Second-model plan review" list of both, because the human is the one
  approving it and a rejected finding is theirs to overrule. A gap only a different
  approach can close goes back to `/approach` under step 2's rule, not into a revision.
- **Second-reviewer options** join `/approach`'s cards on equal terms. One materially distinct
  from every writer option becomes its own card, cut to the card limit; it needs no
  planning-subagent advocate, since the prompt already asked for its strongest case. One
  that matches a writer option merges into that card, with any cost only one side named.
  A credible independent option also closes the single-credible-option exit.
