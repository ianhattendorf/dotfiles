---
name: address-feedback
description: Address feedback on a PR in a GitKraken-owned repo that you authored or are taking over — reviewer comments (human or bot), CI failures, requested changes. Ledger every thread with an explicit disposition, fix-or-decline under ship-issue's rules, gates + comment audit before pushing, replies to humans handed off (never posted). Use when a PR you own has new review feedback, when resuming a feedback round, or when taking over a colleague's or another agent's PR to land it. NOT for reviewing a PR you won't push to — that is /review-change.
---

# Address PR feedback (GitKraken repos)

Read [../ship-issue/rules/host-runtime.md](../ship-issue/rules/host-runtime.md) first for host invocation, questions, planning, and fresh-agent dispatch. Workflow-internal references resolve from the containing document; repository-source pointers resolve from the target checkout. Profiles retain the conventions described in host-runtime.md.


Carry one feedback round from "the PR has comments" to "every thread has a
disposition, fixes are pushed, replies are drafted." You own the branch — this
skill changes code, unlike /review-change — but you do not own the conversation:
replies to humans are handed to the human, never posted. Work inherits the session model except the explicit reviewer selections in the shared rules.

**Resolve the repo profile first**, exactly as ship-issue does: Read
`../ship-issue/profiles/<repo>.md` (derive and write one if missing —
the procedure is in `../ship-issue/SKILL.md`). This skill consumes the
profile's gates, invariants source, live verification method, and merge convention.
Org-wide constants from ship-issue hold here too: writes via the GitKraken MCP,
pushes need explicit human confirmation every time, project-board status is the
human's to set.

## 1. Intake — build the ledger

- Pull the PR detail and ALL reviews/comments (`pull_request_get_detail`,
  `pull_request_get_comments`), plus CI status. Do not work from the human's
  paraphrase of the feedback — read the threads.
- `attach_link` the PR if the task doesn't already carry it. Pull the linked
  issue's acceptance criteria — scope disputes with a reviewer are settled by
  them, same as ship-issue's own rounds.
- Build the **ledger**: every unresolved thread and every red CI check gets a row
  with exactly one disposition —
  - `fix` — the change will be made (record which commit once it exists).
  - `decline` — with a one-line rationale.
  - `question` — needs a reply, no code change.
  - `already-addressed` — point at the existing commit.
  - `out-of-scope` — real but not this PR's problem; file it or name it in the
    reply.
- If a prior round's note exists (step 6), scope to it: triage only threads that
  are new or reopened since the recorded head, and walk the prior ledger's `fix`
  rows to confirm each landed — confirmed, not assumed.

## 2. Ownership fork

How much you must read before touching anything depends on whose diff it is:

- **Own PR, task notes live** (a ship-issue task): context is in the notes —
  proceed to step 3.
- **Own PR, cold** (authored outside a task, or the notes are gone): reconstruct
  intent from the PR body and linked issue before triaging. Do not infer intent
  from the diff alone — the diff is what's being disputed.
- **Take-over** (a colleague's or another agent's PR): pushing to the branch
  adopts its defects as yours. Read the full diff and the entire discussion
  first, then dispatch the review subagent in
  `../ship-issue/rules/fix-or-reject.md` (floor medium) on the whole
  PR, plus the second-model review in that directory's `second-model-review.md` on
  the same range — the first full read of code you did not write is where a second
  family pays most. Both finding sets join the ledger like any reviewer comment. Check for the original author's unpushed
  intent (draft status, WIP markers, TODOs in the diff, a stated plan in the
  discussion) before overriding any of it. Suggest the human post a one-line
  "taking this over" comment — course-changing someone's PR silently is rude
  even when it's sanctioned.

## 3. Checkout freshness

Run `../ship-issue/rules/freshness-check.md`, plus the PR-head check
from /review-change: `git fetch origin`, compare `HEAD` to the PR's current head SHA
(someone may have pushed — the human, a co-author, a suggested-change commit),
fast-forward if behind. If local and remote have diverged, stop and sort it out
with the human before writing anything.

**Rebase policy: append, don't rebase, mid-review.** A force-push destroys the
reviewer's incremental-diff view. Rebase only when `origin/main` moved in a way
that materially affects the PR (conflict, invalidated premise — ship-issue's
freshness classification applies), and when you do, say so in the round's
summary so the reviewer knows why their view reset. Squash-at-merge repos
(kepler) make appending free; a merge-preserving repo's profile governs whether
fixup commits are expected instead.

## 4. Triage and fix

A PR comment is a finding, not an order: triage each by
`../ship-issue/rules/fix-or-reject.md`. Its reject is this ledger's
`decline`.

- Fix `fix` rows grouped **one commit per logical concern**, not one
  "address feedback" mega-commit — the reviewer verifies each thread against
  its commit. Subjects per the profile's merge convention.
- Red CI is a zero-judgment `fix` row: reproduce locally via the profile's
  gates, fix, done.
- Run the profile's gates before anything is pushed, plus a scratch-merge check:
  `git fetch origin`, merge the branch into `origin/main` in a disposable worktree
  (`--no-commit --no-ff`), run at least the typecheck gate there, remove it. Not a
  rebase of the real branch.
- Run that file's review subagent (plus the second-model review, since a push
  follows) on the head you will push. If you fix any of its findings, review again:
  the push waits for a clean round, per ship-issue step 5's convergence rule. The
  reviewer's own fixes are the likeliest source of new findings. A UI-visible fix
  additionally gets the profile's live verification, targeted at what the fix claims.
- Before pushing, run the audit in
  `../ship-issue/rules/comment-audit.md` scoped to every comment
  added or edited this round (`git diff <round-start-head>..HEAD`). A round
  spreads one rationale across the function, the file doc, the test and the
  reply, each feeling non-obvious in isolation — which is why holding the bar
  while writing is not the audit. Apply the verdicts, re-run the gates if
  anything changed, then push.
- Push only with explicit human confirmation, per the org constant.

## 5. Replies

Who replies depends on who commented; this is the canonical statement of the
rule (ship-issue's post-PR step delegates here):

- **Bot/agent comments** (Copilot, CI annotations): the agent may reply
  directly via the MCP, or simply decline. Do not resolve a thread you replied
  to — the tooling can't, and by convention the commenter resolves anyway.
- **Human comments**: the agent NEVER posts the reply. For each thread that
  warrants a response, hand it to the human in chat:
  - TL;DR of the human's comment + a link to it.
  - A suggested response, written to be postable as-is — plain and direct, no
    AI-sounding voice (no "Great point!", no hedging boilerplate, no
    bullet-pointed essay for a one-line question).
  - TL;DR of that suggested response.
- A `fix` reply names the commit that addressed it; a `decline` reply carries
  the rationale. Every ledger row gets a reply or an explicit
  "no reply needed" — an unanswered thread reads as ignored.

Close the round with a summary for the human: the ledger with final
dispositions, cleanly separating what was fixed, what they need to post, and
what was declined and why.

## 6. Wrap up

Record a task note: the ledger (dispositions + commit per `fix` row), the new
head SHA and its merge-base with the target branch, and whether a rebase
happened. The next round scopes against it — new commits only when the recorded
head is an ancestor, `git range-diff` after a force-push (the procedure is
/review-change step 2). Never end a session mid-round with uncommitted work:
checkpoint and note which ledger rows remain.
