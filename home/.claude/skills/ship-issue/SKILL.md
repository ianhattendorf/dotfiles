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
`rules/freshness-check.md`, `rules/fix-or-reject.md`, `rules/comment-audit.md`,
`feature-scale.md`, `profiles/README.md`.

## Never skip

The step bodies state each rule once; this list is the reminder.

- The **freshness check** on every resume, however short the pause felt.
- **Review is a fresh subagent**, never an inline read of your own diff.
- The **comment audit is a fresh subagent** before the first push, never "I held the
  bar while writing".
- The **branch-name check** before the first push.
- **Two turns** for every issue/PR write; **explicit confirmation** for every push.

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
- Issues are assigned to `ianhattendorf`; PRs are never assigned. Project-board status
  cannot be set by tooling — ask the human.
- Task workspace tooling (worktrees, task notes, `attach_link`) works identically in
  every repo.

## 1. Intake & verify

- A task carrying an `/investigate` note has intake and verification done — run the
  freshness check against its baseline sha, then start at issue creation below (the
  note carries a draft title). A task carrying an `/approach` note but no issue: the
  problem statement and acceptance criteria come from that note — file the issue from
  them, then step 2 plans against the same note.
- Pull the source material into context directly: GitKraken MCP tools for issue
  detail, Slack tools for threads. Do not work from a paraphrase.
- Verify the problem is real before anything else: reproduce it, or trace the code
  path to the root cause. Dispatch this to an `Explore`/`general-purpose` subagent once
  it needs more than a couple of targeted reads, so only the root-cause note reaches
  this context. If it cannot be verified, report that and stop.
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
- Refinement happens in the approval loop, *within* the decided approach. Feedback that
  questions the approach itself goes back to `/approach` with the feedback as
  evidence, not into a plan revision. No implementation without approval.
- If planning reveals multiple sessions or phases, switch to `feature-scale.md`.

## 3. Implement

Follow the approved plan. If implementation reveals the plan was wrong in a way that
changes scope or touches a new invariant, return to step 2 rather than improvising.

Write comments to the bar in `rules/comment-audit.md` now, so step 9 is a no-op.

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

After fixing, return to step 4, then review again. **Convergence** is a round with no
findings, or one whose findings are all rejected — cap at ~3 rounds. If rounds
oscillate (a round re-raises what the previous fix introduced), stop patching — the fix
shape is wrong. Escalate one more review subagent to `xhigh` (optionally a different
model) to characterise what the shape cannot accommodate, then take it to `/approach`
with the failed shape recorded as a rejected option and the findings as evidence, and
re-plan (step 2) before writing more code.

## 6. Simplify — once

After correctness converges (never inside the loop), run `/simplify` a single time to
collapse what the fix rounds accreted — code and comments alike. Re-run the gates.
Only if it restructured something substantial, run step 5's subagent once more.

## 7. Automated live verification

For any change with a user-visible surface or an event-producing path, verify live
using the profile's method. Findings go to step 4; return to step 5 only if a fix was
non-trivial.

## 8. Human live run (skippable)

**Skip when** the change is backend-only with no user-visible surface and has direct
test coverage. **Never skip** anything on the profile's never-skip list. State the
decision and reason in one line.

Otherwise hand the human a ready-to-test setup and stop: the app running via the
profile's launch method, and a short script — the 3–5 interactions that exercise the
change and the one or two places a regression would show. Wait for their report;
findings go to step 4.

## 9. Comment audit — final gate

After all reviews and live + human testing, before the first push: run the audit in
`rules/comment-audit.md` over every comment in the diff, apply the verdicts, re-run the
format gate, commit. If it produces substantial deletions, note that as feedback on
step 3, not as a reason to skip it next time.

## 10. PR

- Fetch; rebase if behind `origin/main` — gates on a stale base prove nothing about
  post-merge main. Re-run gates after any rebase.
- All gates green, the profile's extra PR checks done, step 9 actually run as a
  subagent.
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
  otherwise propose; invariants touched; how it was verified (gates + live + human
  run); any noted out-of-scope findings. Describe the shipped end state, not the review
  process — an in-scope bug a round caught and fixed is part of the approach, not a
  callout; that narrative lives in the task notes. Only out-of-scope findings are
  named, because those ask the human for a scope decision.
- `attach_link` the PR and close out the task notes with a final state note.
- Slack-originated task: remind the human in chat to reply to the thread — the
  permalink from the intake note, the PR URL, and a suggested reply if useful. Do not
  post to the thread yourself; the human speaks to humans.

**After the PR is open**, feedback rounds — reviewer comments (human or bot), CI
failures, requested changes — run `/address-feedback`. Do not request bot reviewers —
the pre-PR loop outperforms them, and it is a GitHub write the tooling cannot perform
anyway.
