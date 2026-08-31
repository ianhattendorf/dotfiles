---
name: ship-issue
description: End-to-end loop for shipping a change in a GitKraken-owned repo (Kepler, vscode-gitlens, gkcli, core-tools, …) — intake/verify, plan with human approval, implement, gates, converging code review, simplify pass, live verification, human live run, PR. Use when picking up a GitHub issue, a Slack thread, or a self-noticed problem in a GK repo and carrying it to an open PR. Also use when resuming a task already mid-loop (start at the step the task notes say is next).
---

# Ship an issue (GitKraken repos)

Carry one issue from intake to an open PR. The sequence below is the contract — the
skip criteria and convergence rules are part of it, not suggestions. The sequence is
repo-agnostic; the repo-specific facts each step consumes come from the **repo
profile**. All work inherits the session model — do not pin models or override effort
except where an effort level is written into a step below.

If resuming a task mid-loop, read the task notes (`get_task_context`) first and start at
the step they record as next. Keep a running note of the current step whenever a session
might end between steps. The task note names *which* step is next — it is a pointer, not
a substitute for that step's full text below. Before acting, re-read the step itself in
this file; a paraphrase in the notes can silently drop a bullet (e.g. step 10's branch
rename) that a summary written mid-flow had no reason to repeat in full.

### Freshness check — on every resume

Tasks get paused for days; main moves. Before continuing from the recorded step, run
this (it costs one fetch and two logs — do not skip it because the pause "felt short"):

1. `git fetch origin`, then `git log --oneline $(git merge-base HEAD origin/main)..origin/main --
   <the files/seams the plan names and the branch touches>` — plus a check of the
   linked issue's current state (closed? new comments? linked PRs?).
2. **Nothing relevant landed** → proceed from the recorded step.
3. **Relevant commits landed** → read them and classify:
   - **Premise gone** (the bug was fixed, the feature shipped another way): stop and
     report to the human with the evidence — propose closing the issue and abandoning
     the branch. Do not keep implementing to salvage sunk work.
   - **Plan invalidated** (the seams the plan names were refactored, an invariant
     changed): back to step 2 for a plan revision with human approval.
   - **Mechanical overlap only**: rebase onto `origin/main` NOW, not at PR time —
     conflicts only grow with delay. Re-run gates after; if conflict resolution was
     non-trivial, treat it as a fix and run a review round on the resolution.

Record the check's outcome in the running task note either way, so the next resume
knows the last-verified baseline.

## Repo profile — resolve FIRST

Before any other step: **Read `~/.claude/skills/ship-issue/profiles/<repo>.md`**
(profiles exist for kepler, vscode-gitlens, gkcli). Do not proceed from memory of a
profile — the file is the source of truth, and profiles for different repos contain
opposite instructions (squash vs. merge-preserving, changelog written-in-PR vs.
mined-at-release). Read only the target repo's profile.

Each profile defines: **gates**, **invariants source**, **live verification method**,
**merge convention** (and what checkpoint commits become), **changelog convention**,
**never-skip list** (step 8), **extra PR checks** (step 10). If a profile marks a fact
as unverified or not yet established, verify it in-repo on first use and update the
profile file.

**No profile file for the repo?** Derive one and WRITE it as
`~/.claude/skills/ship-issue/profiles/<repo>.md` (matching the schema above, marking
anything unverified) so every future task inherits it: (1) read
`CLAUDE.md`/`AGENTS.md`/`CONTRIBUTING.md`; (2) gates from `package.json` scripts or
Makefile targets (typecheck/lint/test/format); (3) merge + subject convention by
sampling ~20 `main` subjects (`gh api repos/gitkraken/<repo>/commits`) — squash shows
one commit per PR with `(#NNNN)`; (4) changelog convention (per-version files, single
`CHANGELOG.md`, or none); (5) live verification from repo docs or the `run` skill.

Org-wide constants (hold in every GK repo, no profile needed):

- Issue/PR **writes** go through the GitKraken MCP (`issues_create`,
  `pull_request_create`), never `gh`. In the agent-vm guest that is the only path — its
  `gh` token is deliberately read-only — and on the host, where `gh` *could* write, stay
  on the MCP anyway so the loop behaves identically in both. The MCP covers: kepler,
  vscode-gitlens, gkcli, core-tools, gitkraken-client, api-service, drafts-service,
  gitkraken.dev.
- Pushes go over SSH; the `gh` token plays no part. A push always needs explicit human
  confirmation — that is a permission rule, not a capability limit, so never treat a
  successful `git push` as evidence it was pre-approved.
- Issues are assigned to `ianhattendorf`; PRs are never assigned. Project-board status
  cannot be set by tooling (no MCP surface; the token lacks Projects scope) — ask the
  human to set it.
- Task workspace tooling (worktrees, task notes, `attach_link`) works identically in
  every repo.

## 1. Intake & verify

- Pull the source material into context directly: GitKraken MCP tools for issue detail,
  Slack tools for threads. Do not work from a paraphrase.
- Verify the problem is real before anything else: reproduce it, or trace the code path
  to the root cause. If it cannot be verified, report that and stop — do not proceed on
  an unverified premise.
- If no GitHub issue exists, create one via the GitKraken MCP `issues_create`, per the
  write path above. Print the exact title and body to chat first so the human
  can see what will be created, and pass `assignees: ["ianhattendorf"]`. The issue
  body must include **acceptance criteria** — later review rounds are judged against
  these, and scope disputes are settled by them. Ask the human to set project-board
  status ("In Progress"), once, alongside the creation announcement.
- Attach the issue to the task (`attach_link`).
- Record the verdict as a task note: repro steps, root cause, and explicitly the scope
  you decided NOT to touch. If the task originated from a Slack thread, include the
  thread's permalink in the note — step 10 needs it.

## 2. Plan (skippable for mechanical fixes)

**Skip when** the change is mechanical, single-file, and touches none of the invariants
in the profile's invariants source. When skipping, say so and why in one line.

Otherwise, plan in **plan mode** so the human approves before implementation:

- Start from the verification note — plan the *how*, do not re-investigate the *what*.
- The plan must name: the files/seams to change, **which repo invariants the change
  touches** (check the profile's invariants source), which gates apply, and the
  intended test surface (which test files, which extra gates, what live verification
  will cover).
- Refinement happens in the plan-approval loop. Do not begin implementation without
  approval.

### Feature scale

If planning reveals the work spans multiple sessions or breaks into phases, the plan
becomes a **durable artifact, not a plan-mode plan**: write `PLAN.md` in the task
folder, record a pointer + approval status as a task note, and have the human review
the file itself. A feature-scale plan must contain:

- **Settled decisions** with the reason each was settled (so later sessions don't
  re-litigate them).
- **Phases with sizing**, each independently gateable, and the **demo-able milestone**
  named explicitly.
- **An out-of-scope backlog** (ordered, with known traps documented per item). This is
  the anti-spaghetti rule at feature scale: real-but-deferred concerns get parked here
  by name, never absorbed into the MVP as defensive code.
- **Risks / open items**, including decisions that belong to a human (product calls,
  copy, flag graduation) — flagged, not guessed.
- **Prior-art lessons**, when sibling clients or earlier implementations of the same
  capability exist: survey them before settling decisions, and distill transferable
  lessons and traps into a task note — not just a description of what exists. First
  pass: `gh search code --owner gitkraken`. The likely siblings: **vscode-gitlens**
  (GitLens), **gkcli** (CLI + MCP apps), **gitkraken-client** (the original desktop
  client), **kepler**, and **core-tools** (the shared `@gitkraken/*` libraries; a
  capability shipped as a wrapped library almost always has sibling implementations
  over the same engine). For a deep read, kepler/vscode-gitlens/gkcli/core-tools are
  registered in this workspace — materialize with `create_worktree`, discard once the
  lessons are in a note; read the rest remotely via `gh` or the GitKraken MCP tools.
- **PR strategy**: single PR at the end vs. a PR per phase (prefer per-phase when the
  feature is behind a flag and phases are independently green). Follow the profile's
  changelog convention for how the single user-facing entry lands.

## 3. Implement

Follow the approved plan. If implementation reveals the plan was wrong in a way that
changes scope or touches a new invariant, go back to step 2 rather than improvising.

**Comment discipline (enforced again at step 9 — write to that bar now, so the final
gate is a no-op):** a comment exists only to state what a competent reader cannot get
from the code itself — a constraint, an invariant, surprising behavior, a non-obvious
why. Never write comments that narrate what the code does, reference what was done
("now uses X", "moved from Y"), or justify the change to a reviewer. Say it in one
pass, as briefly as it can be said.

**Feature scale: steps 3–7 run once per phase, not once per feature.** Each phase ends
with: gates green, review converged, a checkpoint commit, and a task note recording
phase status + what's next (sessions may end between phases). Run `/simplify` at each
phase boundary — a multi-day diff simplified only at the end is too late. The human
live run (step 8) happens at the plan's demo-able milestone AND before the final PR;
review findings that are real but deferred go into the plan's backlog with a note,
same as rejected findings.

### Commit strategy

Two rules in every repo:

- **Commit at every converged boundary**: when review converges (step 5), after
  `/simplify` + gates (step 6), after live-verification fixes (steps 7–8). Each is a
  restorable point — a simplify or late fix gone wrong becomes a `git restore`, not an
  untangling.
- **Never end a session with uncommitted work.** Commit a WIP checkpoint and record
  the current step in a task note before stopping.

What checkpoints become at PR time is defined by the profile's merge convention
(squash: checkpoints are disposable and the PR title is the real commit message;
merge-preserving: squash/reword checkpoints locally before the first push).

## 4. Gates

Run the profile's gates before every review round (they are cheap; review rounds are
not). Fix failures and re-run until green. Do not enter review with failing gates.

## 5. Correctness review — converge

Run `/code-review medium`. It reviews the diff with fresh context — never substitute an
in-session "look over what I wrote" pass.

For each finding, decide **fix or reject** by these rules:

- Fix only findings with a **concrete failure scenario**: specific input or state
  leading to a specific wrong output. "Could theoretically" is not a scenario.
- A fix that adds a branch without deleting anything is suspect by default — prefer a
  fix that restructures over one that guards.
- A fix that layers a new comment on top of an existing one is suspect the same way —
  rewrite the comment to state the current invariant in one pass, not accrete another
  justification alongside the last one. A comment that has been edited by two rounds
  and still reads like a log of decisions is a simplify-step finding waiting to happen.
- Every rejected finding gets a one-line rationale in a task note, so later rounds and
  later sessions do not re-raise it. Check that note before triaging a new round's
  findings.
- Out-of-scope findings (real but unrelated) are not fixed here — note them, and file
  or mention them in the PR description instead.

After fixing, return to step 4, then review again. **Convergence** is a round with no
findings, or one whose findings are all rejected under the rules above — cap at ~3
rounds total. If rounds oscillate (a round re-raises what the previous round's fix
introduced), stop patching — the fix shape is wrong; reconsider the approach,
escalating to `/code-review high` with fresh eyes (optionally a different model — this
is the one place model diversity earns its cost) before writing more code.

## 6. Simplify — once

After correctness converges (never inside the loop), run `/simplify` a single time to
collapse what the fix rounds accreted — code AND comments. Any comment touched across
more than one review round is a candidate: reread it cold, as someone with no memory of
the rounds would, and rewrite it to state the current behavior/invariant in one pass
rather than the accumulated justification for it. Then re-run the gates. Only if it
restructured something substantial, run one `/code-review low` pass; otherwise proceed.

## 7. Automated live verification

Verify the change live using the profile's method, for any change with a user-visible
surface or an event-producing path. Findings feed back to step 4 (gates, then fix).
Return to step 5 only if a fix was non-trivial.

## 8. Human live run (skippable)

**Skip when** the change is backend-only with no user-visible surface and has direct
test coverage. **Never skip** anything on the profile's never-skip list. State the
skip decision and reason in one line.

Otherwise, hand the human a ready-to-test setup and stop:

- The app running via the profile's launch method.
- A short test script: the 3–5 interactions that exercise the change, and the one or
  two places a regression would show.
- Then wait. Do not proceed to the PR until the human reports back. Their findings feed
  back to step 4.

## 9. Comment audit — final gate

After all reviews and live + human testing have completed, before the first push:
have **every comment in the diff** read cold. Run the audit as a **fresh-context
subagent** (read-only), not inline — the implementing session wrote these comments
and cannot un-know what they mean; a fresh agent genuinely has no memory of the
plan, the review rounds, or the old code, which is the test. Give it only the diff
plus enough surrounding file context to judge, and the rules below; it returns
per-comment verdicts (keep / delete / trim, one-line reason) — it does not edit.

- Keep a comment only if it tells that reader something true and non-obvious that
  the code cannot: a constraint, an invariant, surprising behavior, a load-bearing
  why. "Not obvious" means not derivable from reading the surrounding code — a
  comment that merely restates or references what was done fails this test.
- Delete comments that narrate the mechanics, describe the change rather than the
  code ("now uses X", "previously Y"), or argue the change's correctness to a
  reviewer — that belongs in the PR body, not the source.
- Trim what survives to its shortest complete form: one pass, no accumulated
  justifications (step 6 should have caught accretion; this gate catches what it
  missed).

Apply the verdicts in the main session, which has the context the subagent lacks.
To keep a comment the cold reader flagged, you must be able to name the concrete
trap it guards — and then the resolution is to **rewrite the comment so the trap is
stated**, not to keep it as-is: a comment whose necessity is invisible to a cold
reader has failed at its one job.

Comment-only edits: re-run the format gate and commit. This pass should be a no-op
if step 3's discipline held — if it produces substantial deletions, note that as
feedback on the implementation pass, not as a reason to skip the gate next time.

## 10. PR

- Ensure the branch is current with `origin/main` (fetch; rebase if behind) — local
  gates on a stale base prove nothing about post-merge main. Re-run gates after any
  rebase.
- All gates (step 4) green, and the profile's extra PR checks done.
- Handle the changelog per the profile's convention (written-in-change: the edit is
  part of this PR; mined-at-release: write nothing).
- Before pushing anything, print a high-level summary of the changes to chat: at
  most 5 bullets of at most 2 sentences each — a maximum, not a target; less is
  more. It MUST state the changelog outcome explicitly: the entry that was added,
  or that none was and why (mined-at-release convention, or no user-facing
  surface).
- Check the issue's acceptance criteria one by one — each is demonstrably met, and the
  PR body says how. If one is not met, that is a scope conversation with the human,
  not a silent omission.
- **STOP before the first `git push` on this branch — this step is easy to skip because
  nothing about a plausible-looking branch name signals it's wrong.** Run
  `git branch --show-current` and check it against `<issue#>-<slug>`: the workspace
  auto-names branches from the task's initial title (or a Slack-thread URL slug), which
  often no longer describes the work. If it doesn't match, rename locally with
  `git branch -m <issue#>-<slug>` (the MCP `git_branch` cannot rename; a local rename
  before any push is safe, though the task metadata keeps the old name, so a later
  `discard_worktree` may leave the renamed branch behind — delete it by hand then).
- Open the PR via the GitKraken MCP `pull_request_create`, per the write path above.
  Do not assign the PR — only the issue gets assigned. Print the exact
  title and body to chat first so the human can see what will be created. The body
  covers: the problem (link the issue), the approach and why, invariants touched, how
  it was verified (gates + live verification + human run), and any noted out-of-scope
  findings.
- Attach the PR to the task (`attach_link`) and close out the task notes with a final
  state note.
- If the task originated from a Slack thread: remind the human, in chat, to reply to
  the thread — include the thread permalink (from the intake note) and the PR URL,
  plus a suggested reply if one is useful. Do not post to the thread yourself (same
  rule as human PR comments: the human speaks to humans).

**After the PR is open:** review feedback — human or bot (Copilot, CI annotations) —
re-enters the loop at step 5's fix-or-reject rules. A PR comment is a finding, not an
order: fix it if it names a concrete failure scenario, otherwise decline it with a
rationale. Fixes go through gates (step 4) and the comment audit bar before pushing.

Who replies depends on who commented. **Bot/agent comments**: the agent may reply
directly, or simply decline. **Human comments**: the agent NEVER posts the reply.
For each human comment that warrants a response, hand it to the human in chat:

- TL;DR of the human's comment + a link to it.
- A suggested response, written to be postable as-is — plain and direct, no
  AI-sounding voice (no "Great point!", no hedging boilerplate, no bullet-pointed
  essay for a one-line question).
- TL;DR of that suggested response.

The human posts it (or doesn't) themselves. Do not request bot reviewers as part of
opening the PR — the pre-PR loop already outperforms them, and requesting reviewers
is a GitHub write the tooling cannot perform anyway.
