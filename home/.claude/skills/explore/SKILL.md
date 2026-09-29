---
name: explore
description: Explore a feature idea for a GitKraken-owned repo BEFORE it is a problem report or an issue — flesh out a half-formed "I'd like to have…" through discussion grounded in what the product already does, steelman dropping it, and end with a drop / park / commit verdict recorded as a durable task note that /ship-issue step 1 files the issue from. Use when the human has an idea whose what is still open, or wants to decide whether they still want something they once wanted. NOT for a reported or suspected problem (that is /investigate), NOT for choosing between ways to build a settled feature (that is /approach), and NOT for work already committed to (that starts in /ship-issue).
---

# Explore (GitKraken repos)

Answer one question — **what is this, and do I still want it?** — and leave behind a
note that a future session can pick up without re-having the conversation. This skill
ends at the verdict. It files nothing, chooses no approach, plans nothing, builds nothing.

Unlike its neighbours this is a **conversation, not a procedure**. The inputs are not
settled: the idea changes shape as it is discussed, and "I no longer want this" is a
successful outcome, not a failed one. The fixed parts are the questions that must get
asked, the grounding that must be done, and the note that must be left.

Hard limits (the point of this skill vs. its neighbours):

- **No GitHub issue is created**, and no existing issue is commented on. A `commit`
  verdict drafts the title and the acceptance criteria; `/ship-issue` step 1 files it.
  A dropped idea never touches GitHub.
- **No option cards.** Noticing that the idea could be built more than one way is in
  scope and goes in the note as a fork; weighing the options is `/approach`, which
  `/ship-issue` step 2 runs itself when it is needed.
- **No code changes**, except a throwaway probe (a Storybook story, a read-only spike)
  reverted before the session ends and summarised in the note.
- **Not a cheerleader.** The idea is the human's; the job is to make it sharper or to
  help them let it go, and both require pushing back. Once the human decides, the
  decision is theirs — the note makes it stick, it does not argue with it.

## 1. Frame

- Read the task notes (`get_task_context`). **One task per idea** — an existing
  `/explore` note means this is a resume: read it, ask what has changed since, and
  update that note (`update_task_note`) rather than adding a second one. A second idea
  that surfaces mid-conversation gets its own task (`create_task`), not a paragraph.
- Take the idea in the human's own words and write it back in one line. If it will not
  compress to one line, that is the first thing to discuss.
- Read the **invariants source** section of the repo profile
  (`~/.claude/skills/ship-issue/profiles/<repo>.md`) — that section only — so that an
  idea colliding with a load-bearing rule is noticed here, not at plan time.

## 2. Ground

Abstract feature talk drifts; an idea sharpens fastest against what exists. Dispatch
each survey to a subagent so only the distilled facts reach this context:

- **What the product already does near this** — the features, settings, commands and
  docs adjacent to the idea (an `Explore` subagent over the repo). The answer is one of
  "nothing", "half of it, here", or "all of it, spelled differently", and each changes
  the conversation.
- **Whether someone has already asked** — a keyword search over open and recently
  closed issues. An existing issue is context, not the task; do not `attach_link` it
  unless the idea turns out to *be* that issue.
- **Prior art in sibling clients** when the capability is one they might have solved
  (vscode-gitlens, gkcli, gitkraken-client, core-tools; first pass
  `gh search code --owner gitkraken`), one `general-purpose` subagent per sibling. The
  lesson is what comes back, not a description of the sibling's implementation.

Run the first two before the conversation starts; the third when the discussion needs
it. Bring the results in as facts, in a few lines each.

## 3. Converse

Ask in chat, one or two questions at a time, and let the answers steer — this is the
part that cannot be a checklist. The questions that must be answered before a verdict,
in whatever order the conversation reaches them:

- **Who feels this today, and what do they do instead?** "Nobody, I just want it" is an
  allowed answer; write it down as such, because it is what a later reviewer of the
  issue will ask.
- **Is it a gap or a duplication?** Hold the idea against the grounding from step 2.
  Half of it existing is usually the most useful finding of the session.
- **What is the smallest version that would tell you whether you still want it?** This
  is what becomes the acceptance criteria — not the fullest version.
- **What is it not?** Scope fences, named. Every one written here is a scope dispute
  that does not happen in a review round.
- **What would make you drop it?** Ask this once, directly, and then **steelman
  dropping it** yourself in a few lines before any `commit` verdict: the cost of
  carrying the feature, what it forecloses, what it duplicates. If the case for
  dropping is stronger than the case for building, say so plainly. The human decides.

Use `AskUserQuestion` only for a genuine forced choice with named options; open
questions go in prose.

**When talk stalls, probe.** A discussion stuck on "would this even feel right" or "is
this even possible" is not going to be settled by more talk. For a UI idea the first
probe is a `/design` canvas (`~/.claude/skills/ship-issue/rules/design-canvas.md`): it
shows several options and touches no code. A throwaway Storybook story, or a read-only
feasibility spike under ~30 minutes, is the probe when the canvas is unavailable or the
question is about behavior. Revert code probes before the session ends; what any probe
showed goes in the note.

**Time-box the session, not the idea.** Every session ends with a note, whatever the
state of the thinking. "Still thinking" is a `park` verdict with the open question that
would move it, never a missing note.

## 4. Verdict + note

Report the verdict to the human in chat (the deliverable is the assessment), and
record it with `add_task_note` (or `update_task_note` on a resume). The note's shape is
the interface `/ship-issue` step 1 consumes on `commit` — all ten parts, in order:

1. **Idea**: one line, in the human's words.
2. **Problem, and who has it**: or "no problem — wanted for its own sake".
3. **Done looks like**: the acceptance criteria for the smallest version, as a list.
   On `commit` these become the issue body's acceptance criteria verbatim.
4. **Not this**: the scope fences.
5. **Grounding**: what exists nearby, existing issues, prior-art lessons, and what a
   probe showed, if one ran (for a `/design` canvas: its URL and the artboard picked).
6. **Open questions**: what is still unsettled, and who settles it (the human, a user,
   a spike). On `commit`, these go to the plan's risks list, not into the MVP.
7. **Approach fork**: the credible ways to build it that surfaced, one line each, or
   "none seen". This is what ship-issue step 2 reads to decide whether `/approach` is
   needed; the note records the fork, it does not route on it.
8. **Verdict**: one of `drop` (the reason, so it does not come back next month) /
   `park` (the one question or event that would revive it) / `commit` (a draft issue
   title).
9. **Baseline**: repo, commit sha, date — ship-issue's freshness check re-validates
   the grounding against this.
10. **Next step**: on `commit`, "invoke /ship-issue; explore note exists — file the
    issue from it, then step 2 plans against it". On `park`, "revisit when <the
    condition in 8>". On `drop`, "none". Never "invoke /approach first".

If the verdict is `commit` and the human says to proceed, that is `/ship-issue` from
here on — this skill does not continue into it.
