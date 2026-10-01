---
name: investigate
description: Investigate a reported or suspected problem in a GitKraken-owned repo (a Slack post, a Sentry event, a hunch, an existing issue) WITHOUT committing to ship it — reproduce or trace to root cause, then record a verdict as a durable task note that /ship-issue can later pick up as completed intake. Use when someone wants to understand whether a problem is real before deciding to file an issue or start implementation. NOT for work already committed to — that starts (or resumes) directly in /ship-issue.
---

# Investigate (GitKraken repos)

Read [../ship-issue/rules/host-runtime.md](../ship-issue/rules/host-runtime.md) first for host invocation, questions, planning, and fresh-agent dispatch. Workflow-internal references resolve from the containing document; repository-source pointers resolve from the target checkout. Profiles retain the conventions described in host-runtime.md.


Answer one question — **is this real, and what is it?** — and leave behind a note
good enough that a future `/ship-issue` session never re-derives the answer. This
skill ends at the verdict. It files nothing, plans nothing, fixes nothing.

Hard limits (the point of this skill vs. ship-issue):

- **No GitHub issue is created.** If the verdict is "worth an issue", the note says
  so and drafts the title — the human decides when, by invoking `/ship-issue`.
- **No fix, however small.** A one-line fix discovered during investigation goes in
  the note as the proposed fix, not into the tree. Read-only on the repo, except
  throwaway repro scaffolding that is reverted before the session ends.
- **No plan, and no approach.** Naming the root-cause seam is in scope; choosing
  between ways to build the fix is `/approach`, and designing the chosen one is
  step 2 of ship-issue.

## 1. Intake

- Pull the source material into context directly — Slack tools for a thread,
  GitKraken MCP for an existing issue, Sentry tools for an event. Do not work from
  a paraphrase, including the human's own summary of the thread.
- If the investigation centers on an existing GitHub issue or PR, attach it to the
  task (`attach_link`). A Slack thread's permalink goes in the note instead.
- If repro needs a running app or the code, read the repo profile
  (`../ship-issue/profiles/<repo>.md`) for the **live verification
  method** and use only that section — the rest of the profile (merge, changelog,
  PR checks) is ship-issue's concern. Prefer the task's existing worktree; create
  one only if repro requires it, and say in the note whether it should be kept for
  the fix or discarded.

## 2. Verify

- Reproduce the problem, or trace the code path to the root cause. One or the
  other must be concrete: a repro is steps + observed-vs-expected; a trace names
  the files/seams and the specific wrong behavior at them.
- Time-box a failing repro: if it will not reproduce after a genuine attempt,
  "not reproducible" IS a verdict — record what was tried and what would be needed
  (an account state, a specific platform, more logs from the reporter). Do not
  keep escalating effort past the point where the next step is a question for the
  reporter.
- Note adjacent problems you trip over, but do not chase them — they go in the
  note's out-of-scope list.

## 3. Verdict + note

Report the verdict to the human in chat (the deliverable is the assessment), and
record it as a task note (`add_task_note`). The note's shape is the interface
`/ship-issue` step 1 consumes — all seven parts, in this order:

1. **Source**: permalink to the Slack thread / Sentry event / issue.
2. **Verdict**: one of `real — worth an issue` (include a draft issue title) /
   `real — not worth fixing` (say why) / `not reproducible` (say what was tried
   and what's needed) / `needs a human decision` (name the decision).
3. **Evidence**: the repro steps, or the traced path with file:line seams.
4. **Root cause**, as specifically as the evidence supports.
5. **Out of scope**: what you explicitly did NOT dig into, and adjacent problems
   noticed but not chased.
6. **Baseline**: repo, commit sha investigated at, and the date — ship-issue's
   freshness check re-validates against this before an issue is filed on it.
7. **Next step**: "invoke /ship-issue; intake note exists — start at issue
   creation", or "reply to the reporter asking for X", or "no action". Never "invoke
   /approach first": ship-issue step 2 decides whether the approach is contested and
   runs `/approach` itself. If you saw more than one credible way to build the fix,
   record the fork and its options under **Approach fork** in the note — that is what
   step 2 reads to decide — and still route to `/ship-issue`.

If the verdict is `real — worth an issue` and the human says to proceed, that is
`/ship-issue` from here on — this skill does not continue into it.
