---
name: ship-issue
description: End-to-end loop for shipping a change in a GitKraken-owned repo (Kepler, vscode-gitlens, gkcli, core-tools, …) — intake/verify, approach decision when the how is open, plan with human approval, implement, gates, converging code review, simplify pass, live verification, human live run, PR. Use when picking up a GitHub issue, a Slack thread, or a self-noticed problem in a GK repo and carrying it to an open PR. Also use when resuming a task already mid-loop (start at the step the task notes say is next).
---

# Ship an issue (GitKraken repos)

Carry one issue from intake to an open PR. The sequence below is the contract — the
skip criteria and convergence rules are part of it. The sequence is repo-agnostic;
the repo-specific facts each step consumes come from the **repo profile**. All work
inherits the session model — no pinned models or effort overrides except where a step
writes one in.

Shared procedures live beside this file and are read when a step calls for them:
`rules/freshness-check.md`, `rules/fix-or-reject.md`, `rules/second-model-review.md`,
`rules/comment-audit.md`, `feature-scale.md`, `profiles/README.md`. `../README.md` routes between this skill and
its neighbours (`/investigate`, `/approach`, `/review-change`, `/address-feedback`).

## Never skip

The step bodies state each rule once; this list is the reminder.

- The **freshness check** on every resume, however short the pause felt.
- **Review is a fresh subagent**, never an inline read of your own diff.
- The **second-model review on every plan before approval and on the final pre-push
  round**; a Codex run that could not run is blocking, not skipped.
- The **comment audit is a fresh subagent** before the first push, never "I held the
  bar while writing".
- The **branch-name check** before the first push.
- **Two turns** for every issue/PR write; **explicit confirmation** for every push.
- **Every human decision goes through `AskUserQuestion`**, never a question in prose.
- The **scratch-merge gate** against current `origin/main` before every push.
- The **intake repro, replayed** before the PR — the acceptance criteria are not a
  substitute for re-running what started the task.
- **A step that could not run is blocking, not a silent skip.** Stop and say what you
  tried; never substitute weaker verification and report the step as done. A degraded
  step looks identical to a passed one in the final report, which is what makes this the
  easiest rule in the list to break and the most expensive.

## Resuming

Read the task notes (`get_task_context`) first and start at the step they record as
next. The note is a pointer — re-read the step's full text here before acting, since a
paraphrase drops bullets. Then run `rules/freshness-check.md`. Keep a running note of
the current step whenever a session might end between steps.

## Repo profile — resolve first

Read `~/.claude/skills/ship-issue/profiles/<repo>.md` for the target repo only. Do not
proceed from memory: profiles for different repos contain opposite instructions (squash
vs. merge-preserving, changelog in-PR vs. mined-at-release). No profile? Derive and
write one per `profiles/README.md`.

Org-wide constants (hold in every GK repo, no profile needed):

- Issue/PR **writes** go through the GitKraken MCP (`issues_create`,
  `pull_request_create`), never `gh` — the guest's `gh` token is read-only, and the
  host stays on the MCP so the loop behaves identically in both. The MCP covers:
  kepler, vscode-gitlens, gkcli, core-tools, gitkraken-client, api-service,
  drafts-service, gitkraken.dev.
- Every issue/PR write is confirmed in **two turns, in this order**. First, a chat
  message carrying the draft: the title on its own line, then the body, all inside one
  fenced code block (a longer fence than any inside) so the human can copy the raw
  markdown to edit it. Second, `AskUserQuestion` with a one-sentence question pointing
  back at the draft — "File the issue drafted above?" / "Open the PR drafted above?" —
  and two options, e.g. "File it as drafted" (recommended) / "Let me edit it first".
  The question and option text never carry the title or body: that field renders
  plain text, cannot be copied, and vanishes when the question resolves. If the draft
  is not already in chat, print it first; never fold the two turns into one.
- Pushes go over SSH and always need explicit human confirmation — a permission rule,
  not a capability limit; a successful `git push` is not evidence it was pre-approved.
- **A decision that is the human's to make is posed with `AskUserQuestion`** — the
  named ones above, and equally any that arises mid-loop and is not covered by a step.
  Never as a question in chat prose: prose does not block, so the loop reads the next
  instruction as an answer and decides by default. The same holds when a step routes
  the decision elsewhere (step 2 sends a contested approach to `/approach`) — take that
  route, and if there is a reason not to, the choice between routes is itself a
  question for the human, asked with the tool.
- **Anything the human must read before answering goes in a message that ends the
  turn**, never in text written just before the question in the same turn. Kepler's
  transcript can hold back text written before a tool call and show only the question
  card, so a card that says "the URLs above" can refer to text the human never saw. A
  card that asks within one turn must stand on its own: its question and option text
  carry everything needed to answer.
- Issues are assigned to `ianhattendorf` — on a pre-existing issue that means checking
  the assignee at intake, not only setting it at creation. PRs are never assigned.
  Project-board status cannot be set by tooling — ask the human.
- Task workspace tooling (worktrees, task notes, `attach_link`) works identically in
  every repo.

## 1. Intake & verify

- A task carrying an `/investigate` note has intake and verification done — run the
  freshness check against its baseline sha, then start at issue creation below (the
  note carries a draft title). A task carrying an `/approach` note but no issue: the
  problem statement and acceptance criteria come from that note — file the issue from
  them, then step 2 plans against the same note. A task carrying an `/explore` note
  with a `commit` verdict is the same case: its "Done looks like" list is the
  acceptance criteria, its "Not this" list is the scope you decided not to touch, and
  its "Approach fork" is what step 2 reads.
- Pull the source material into context directly: GitKraken MCP tools for issue
  detail, Slack tools for threads. Do not work from a paraphrase.
- Verify the problem is real before anything else: reproduce it, or trace the code
  path to the root cause. Dispatch this to an `Explore`/`general-purpose` subagent once
  it needs more than a couple of targeted reads, so only the root-cause note reaches
  this context. If it cannot be verified, report that and stop.
- Before filing, search for an existing issue (`issues_get_detail` on anything the
  source material references, plus a keyword search over open AND recently closed
  issues). A duplicate is a comment on the original, not a second issue; a recently
  closed one that describes this exact problem is evidence for the freshness check, not
  a reason to stop.
- When the issue already exists, check its assignee in the `issues_get_detail` result.
  If it is unassigned or assigned to someone else, stop and ask with `AskUserQuestion`
  before any implementation work — options "Self-assign and continue" (recommended),
  "Continue without assigning", and, when someone else holds it, "Stop — it's theirs".
  Someone else's assignment is a signal they may already be working it, and an
  unassigned issue shipped to a PR leaves no record of who took it. On "self-assign",
  write the assignee through the GitKraken MCP; if no tool there can set it, hand the
  human the exact change to make instead of substituting a comment.
- If no GitHub issue exists, create one via `issues_create` per the write path above,
  with `assignees: ["ianhattendorf"]`. The body must include **acceptance criteria** —
  review rounds are judged against them and scope disputes are settled by them. Ask
  the human once to set project-board status ("In Progress").
- `attach_link` the issue.
- Record a task note: repro steps, root cause, and explicitly the scope you decided
  NOT to touch. If the task came from a Slack thread, include its permalink — step 10
  needs it.

## 2. Plan (skippable for mechanical fixes)

**Skip when** the change is mechanical, single-file, and touches none of the invariants
in the profile's invariants source. Say so, and why, in one line.

### Decide the approach before planning it

A plan bakes in an approach — by the time it names files and seams, the how has been
chosen by whichever seam was found first — and plan approval is a yes/no on one plan,
the wrong shape for a choice between approaches. Before plan mode, check the task for
an **approach decision note** (from `/approach`):

- **A note exists** → plan against it. Its rejected options are settled.
- **No note, and any of these hold** → run `/approach` first and come back with its
  note: more than one credible seam could host the change; the change touches an
  invariant in the profile's source; it introduces a new concept (an event, a table or
  migration, a setting, a flag, a wire route); the intake verdict was "needs a human
  decision"; or the human asked to settle direction first.
- **Otherwise** → say in one line that the approach is uncontested and what it is.

Then plan in **plan mode** so the human approves before implementation:

- Start from the verification note (and the approach note) — plan the *how*, do not
  re-investigate the *what* or re-decide the approach.
- Name: the files/seams to change, **which repo invariants the change touches**, which
  gates apply, and the intended test surface (test files, extra gates, what live
  verification covers).
- **A UI change names its visual target**: the `/design` artboard an explore or
  approach note picked, or, when no note settled the visual shape, one produced now
  (`rules/design-canvas.md`). A change the existing components already dictate needs
  none; say so.
- **Before asking for approval, run the second-model plan review**
  (`rules/second-model-review.md`, plan-review form) and triage it per that file; the
  plan you present carries its outcome. A revision re-runs it only when it changes the
  files, seams or invariants the plan names.
- Refinement happens in the approval loop, *within* the decided approach. Feedback that
  questions the approach itself goes back to `/approach` with the feedback as
  evidence, not into a plan revision. No implementation without approval.
- If planning reveals multiple sessions or phases, switch to `feature-scale.md`.

## 3. Implement

Follow the approved plan. If implementation reveals the plan was wrong in a way that
changes scope or touches a new invariant, return to step 2 rather than improvising.

Write comments to the bar in `rules/comment-audit.md` now, so step 9 is a no-op.

### Prove the test covers the bug

A test added for a fix **must be observed failing without the fix**. Run it against the
pre-fix tree — `git stash push -u` the fix, or run the test at the parent commit in a
disposable worktree — record the failure output, then confirm it passes with the fix.
Paste both outcomes into the task note.

This is the one claim in the loop that is otherwise taken on assertion. A test written
after the fix, against the fixed code, passes whether or not it exercises the defect:
the profile's "derive the covering set by grepping the changed identifiers" finds tests
that BREAK, and nothing else establishes that the new test would have CAUGHT this. A
test that passes on the pre-fix tree is not a regression test — fix the test, not the
record.

Skip only when the change has no testable surface (a comment, a doc, a string), and say
so in one line.

### Commit strategy

- **Commit at every converged boundary**: review converged (step 5), after
  `/simplify` + gates (step 6), after live-verification fixes (steps 7–8). Each is a
  restorable point — a simplify or late fix gone wrong becomes a `git restore`.
- **Never end a session with uncommitted work.** Commit a WIP checkpoint and record
  the current step in a task note before stopping.

What checkpoints become at PR time is the profile's merge convention (squash:
checkpoints are disposable and the PR title is the real commit message;
merge-preserving: squash/reword checkpoints locally before the first push).

## 4. Gates

Run the profile's gates before every review round (gates are cheap; review rounds are
not). Fix failures and re-run until green.

**Before every push, also gate against a scratch merge with current `origin/main`** —
a clean local gate run and a clean rebase can both miss upstream drift in code the
branch doesn't touch. `git fetch origin`, then in a disposable worktree:
`git worktree add <tmp> origin/main && git -C <tmp> merge <branch> --no-commit --no-ff`,
run at least the typecheck gate there, remove the worktree. Not a rebase of the real
branch — no force-push, no reviewer-view reset.

## 5. Correctness review — converge

Each round, dispatch the review subagent in `rules/fix-or-reject.md`, floor **medium**,
and triage its findings by that file's rules. Skip a round only when every hunk in the
diff is comment-only, i18n-string-only, or pure formatting.

When the diff touches a **security-sensitive surface** from the profile's list — or
adds/changes a direct dependency or the lockfile — run `/security-review` once, after
correctness converges and before step 6. Its findings triage by the same
`rules/fix-or-reject.md` rules as any other round.

After fixing, return to step 4, then review again. **Convergence** is a round with no
findings, or one whose findings are all rejected, **on the head you will push**. A
round whose findings you fixed has not converged, however small the fixes. Its fixes
are unreviewed code and get one more round. The same applies to anything that changes
code after convergence: step 6, step 8's fixes, a pre-push nit. The exception is
step 9's comment-only edits, which need only the format gate. Cap at ~3 rounds. Hitting
the cap without a clean round is the oscillation signal below, not permission to push.
If rounds
oscillate (a round re-raises what the previous fix introduced), stop patching — the fix
shape is wrong. Escalate one more review subagent to `xhigh` and, beside it, the
second-model review (`rules/second-model-review.md`, effort `xhigh`) to characterise
what the shape cannot accommodate, then take it to `/approach`
with the failed shape recorded as a rejected option and the findings as evidence, and
re-plan (step 2) before writing more code.

## 6. Simplify — once

After correctness converges (never inside the loop), run `/simplify` a single time to
collapse what the fix rounds accreted — code and comments alike. Re-run the gates.
If it changed code, run one more step 5 round. Step 5's convergence rule applies.

## 7. Automated live verification

For any change with a user-visible surface or an event-producing path, verify live
using the profile's method. Findings go to step 4, and a code fix goes back through
step 5 (its convergence rule covers fixes made after convergence).

**Ask the live tool for findings only, explicitly, in the invocation** — the profile's
live-verification entry gives the exact form. It reports; this loop fixes. These tools default to
fixing what they find, and fixes dispatched from inside one bypass steps 4 and 5 and
are gated against its list rather than the profile's. Unasked, it will fix.

If the tool writes artifacts into the repo worktree (Kepler's `/live-exercise` writes
`.tasks/<feature>-exercise/`), **fold them into a task note before moving on**:
findings with their dispositions, and every "liberty taken" entry, which step 10's PR
body needs. Those files are gitignored and worktree-local — they do not survive a
`discard_worktree`, and a resume reads task notes, not them.

### 7b. Replay the intake repro

Re-run the reproduction from the step 1 note against the fixed build and record the
result in the task note. Step 10 cites it. Acceptance criteria are checked separately
and are not a substitute: they are what the fix was supposed to achieve, and the repro
is what actually happened to someone.

## 8. Human live run (skippable)

**Skip when** the change is backend-only with no user-visible surface and has direct
test coverage. **Never skip** anything on the profile's never-skip list. State the
decision and reason in one line.

Otherwise hand the human a ready-to-test setup and stop: the app running via the
profile's launch method, and a short script — the 3–5 interactions that exercise the
change and the one or two places a regression would show. If the plan names a
`/design` artboard, link it so the human can compare the build against it. Wait for their report;
findings go to step 4.

**If they are not there, park — do not idle.** Commit the checkpoint, record in the task
note that the handoff is pending and what was handed over (the URL, the script, the
instance and how to stop it), and end the session. The same applies to any blocking
wait in this loop, including the two-turn issue/PR confirmations: a waiting session
burns a live context to no purpose, and the note is what lets the next one resume in one
read.

## 9. Comment audit — final gate

After all reviews and live + human testing, before the first push: run the audit in
`rules/comment-audit.md` over every comment in the diff, apply the verdicts, re-run the
format gate, commit. If it produces substantial deletions, note that as feedback on
step 3, not as a reason to skip it next time.

## 10. PR

- Fetch; rebase if behind `origin/main` — gates on a stale base prove nothing about
  post-merge main. Re-run gates after any rebase.
- All gates green, the profile's extra PR checks done, step 9 actually run as a
  subagent, step 7b's repro replay recorded.
- Changelog per the profile's convention (written-in-change: the edit is part of this
  PR; mined-at-release: write nothing).
- Print a high-level summary to chat: at most 5 bullets of at most 2 sentences — a
  maximum, not a target. It states the changelog outcome explicitly: the entry added,
  or that none was and why.
- Check the acceptance criteria one by one — each demonstrably met, and the PR body
  says how. An unmet one is a scope conversation with the human, not a silent omission.
- Before the first push, check `git branch --show-current` against `<issue#>-<slug>`:
  the workspace auto-names branches from the task's initial title, which often no
  longer describes the work. Rename locally with `git branch -m` (the MCP `git_branch`
  cannot rename; task metadata keeps the old name, so a later `discard_worktree` may
  leave the renamed branch to delete by hand).
- Open the PR via `pull_request_create` per the write path above. No assignee. The
  body covers: the problem (link the issue); the approach and why, lifted from the
  approach note with one line per rejected option — those are what a reviewer would
  otherwise propose; invariants touched; how it was verified (gates + the replayed
  intake repro + live + human run, and for a fix, the test observed failing without it); any noted out-of-scope findings. Describe the shipped end state, not the review
  process — an in-scope bug a round caught and fixed is part of the approach, not a
  callout; that narrative lives in the task notes. Only out-of-scope findings are
  named, because those ask the human for a scope decision.
- `attach_link` the PR and close out the task notes with a final state note.
- Slack-originated task: remind the human in chat to reply to the thread — the
  permalink from the intake note, the PR URL, and a suggested reply if useful. Do not
  post to the thread yourself; the human speaks to humans.

## 11. Teardown

The loop leaves running processes and disposable checkouts behind; a task that ends
without clearing them costs the next one a port, a lock, or a confusing worktree.

- Stop every live instance this task started, **using the profile's stop command** —
  never a process-name match. Names overlap (Kepler's `kepler-serve` is a prefix of the
  real `kepler-server` daemon), and the wrong match kills the human's own work.
- Remove any scratch-merge worktrees; confirm none are left with `git worktree list`.
- Confirm no background agents or background Bash processes are still running.
- The task worktree stays — the PR is open against it. Say in the final note what was
  left running deliberately and why.

State what was torn down in one line. "Nothing was running" is a valid line; silence is
not, because a leaked instance is invisible until it collides.

**After the PR is open**, feedback rounds — reviewer comments (human or bot), CI
failures, requested changes — run `/address-feedback`. Do not request bot reviewers —
the pre-PR loop outperforms them, and it is a GitHub write the tooling cannot perform
anyway.
