---
name: review-change
description: Review a change in a GitKraken-owned repo (Kepler, vscode-gitlens, gkcli, …) — a PR, a branch, a commit/range, or a raw diff. Intake with existing-comment dedup (PRs), gates on the checkout, /code-review + repo-invariant pass, live verification when warranted, findings delivered as a human handoff (never posted directly to a colleague's PR). Use when asked to review a PR, branch, commit, or diff, when a task has a PR link attached for review, or for a second opinion on a merged/open change. NOT for reviewing your own in-progress diff mid-ship-issue — that loop has its own review steps.
---

# Review a change (GitKraken repos)

Produce a review of a change — a PR, a branch, a commit/range, or a raw diff:
verdict + findings, delivered to the human. You are a reviewer, not a fixer — this
skill changes nothing in the target and posts nothing anywhere. All work inherits
the session model.

**Resolve the repo profile first**, exactly as ship-issue does: Read
`~/.claude/skills/ship-issue/profiles/<repo>.md` (derive and write one if missing —
the procedure is in `~/.claude/skills/ship-issue/SKILL.md`). The review consumes the
profile's gates, invariants source, and live verification method.

## 1. Resolve the target

The target type determines what the review can include. Degradations are **named in
the verdict**, never silently absorbed — a static-only review is a different
confidence level than a full one, and the human must know which they got.

- **PR** (URL/number, or attached to the task): full protocol. `attach_link` it if
  the task is about it. Pull the PR detail AND all existing reviews/comments
  (`pull_request_get_detail`, `pull_request_get_comments`) — anything a previous
  reviewer already raised is off the findings list; read the discussion before
  forming opinions, not after. Spec = the linked issue's acceptance criteria.
- **Branch or commit/range**: diff is `merge-base(target-branch)..head` (or the
  given range). No discussion to dedup. Spec: a linked issue if one is referenced;
  otherwise ask the human what the change is meant to do — or, if they want a cold
  read, review against correctness + invariants only and say so in the verdict.
- **Raw diff/patch**: no checkout of the change → no gates, no live verification
  (steps 3, 5, 6 don't apply — static review only, named in the verdict).
  Uncommitted working-tree changes are NOT this case: the dirty tree is a checkout;
  treat it as a branch.

Whatever the target: establish intent before hunting defects. Flawless code solving
the wrong problem is a finding, and often the most valuable one.

## 2. Checkout state (targets with a checkout)

For a PR, Kepler normally provisions the task worktree already checked out on the
PR branch. Verify rather than assume: `git fetch origin`, then compare `HEAD` to
the PR's current head SHA (from the PR detail) — commits may have been pushed since
the worktree was created. If HEAD is behind, fast-forward; if the worktree is on
some other branch entirely, say so and sort it out with the human before reviewing
the wrong code. For a branch or commit target, run the same check against the named
ref.

If a prior review note exists (step 8), this is a **re-review** — scope it instead
of starting over:

- Recorded head still an ancestor of the new head → review only `recorded..HEAD`.
- Not an ancestor (rebase / force push) → `git range-diff <old-base>..<old-head>
  <new-base>..HEAD` using the SHAs from the note. All-equal output means a clean
  rebase: just verify the prior findings were addressed. Content changed, or the
  old objects are no longer fetchable → re-review the full diff, walking the prior
  findings as a checklist first so resolutions are confirmed, not assumed.

## 3. Gates

Run the profile's gates on the checkout. A failing gate is a finding that costs zero
judgment — report it precisely (command + output), don't fix it. (Checkout-less
targets: skipped — already flagged as a verdict caveat by step 1.)

## 4. Review

- Run `/code-review high` against the target. Higher effort than ship-issue's inner
  loop deliberately: this may be the only review the change gets from you. For
  large or high-stakes PRs (security-adjacent, invariant-heavy, >~1k lines), run
  `xhigh` instead — accept the extra triage noise there and only there.
- Make an explicit pass against the profile's invariants source — enforced and
  load-bearing rules first (the ones with residual tests are cheap to check; the
  ones without are where a reviewer earns their keep).
- Walk the spec from step 1 (acceptance criteria, or the stated intent): is each
  part demonstrably met by this diff? Unmet or silently-narrowed scope is a finding.
- Findings bar is the same one ship-issue holds its own work to: a concrete failure
  scenario (specific input/state → wrong output), not "could theoretically". Do not
  nitpick style a colleague's repo conventions permit, and do not re-raise anything
  from the existing discussion (step 1).

## 5. Live verification (when warranted)

For UI or event-producing changes, verify the change's claims (PR body, commit
message, or stated intent) live using the profile's method (Kepler: `serve:isolate`
+ `/live-exercise`-style spot checks). This is targeted — exercise what the change
claims to do and the one or two places a regression would show, not a full audit.

## 6. Human UI pass (optional)

Offer this when the change touches anything on the profile's never-skip list (UI,
event-flow, terminal/agent paths) and look-and-feel judgment matters to the verdict
— a reviewer approving UI they never saw is signing someone else's work. High level:

1. Launch the change live (Kepler: `pnpm serve:isolate` in this worktree) and
   print the Remote access URL to chat.
2. Hand the human a walk script: the 3–5 interactions the PR claims to change, and
   the one or two places a regression would show.
3. Fold their impressions into the verdict — anything broken is a finding like any
   other.

Skip silently when the change has no user-visible surface.

## 7. Deliver — human handoff

Nothing is posted anywhere. Produce in chat:

- **Verdict**: one line — approve / approve-with-nits / needs-changes, and why —
  plus any step-1 degradation caveats (static-only, intent unverified).
- **Per finding**: TL;DR of the problem, a `file:line` link, and suggested feedback
  written to be sent as-is through whatever channel fits (PR comment, Slack reply
  to the author) — plain and direct, non-AI voice (no "Great work overall!", no
  hedging boilerplate), phrased as a colleague would ask it.
- Cleanly separate **blocking findings** from **nits** so the human can post the
  first set and drop the second without re-triaging.

The human posts what they agree with, edits or drops the rest. (Second opinion on a
PR from one of your own tasks? Skip the handoff — feed findings into that task's
fix loop.)

## 8. Wrap up

Record the verdict + findings summary as a task note (future sessions on this task
should not re-review from scratch), including the reviewed **head SHA and its
merge-base with the target branch** — step 2 needs both to scope a re-review after
new commits or a rebase/force push. For a checkout-less diff, record what
identifies the content instead (patch path + hash).
