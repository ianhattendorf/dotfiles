---
name: approach
description: Decide the high-level approach for a change in a GitKraken-owned repo BEFORE it is planned or implemented — enumerate two or three credible options at altitude, weigh them against the acceptance criteria and the repo's invariants, get the human's pick, and record the decision as a durable task note that /ship-issue step 2 plans against. Use when a verified problem or a stated feature could be built more than one way, when the human wants direction settled before seeing a plan, or when a /ship-issue review loop oscillates and the fix shape itself is in question. NOT for deciding whether a problem is real (that is /investigate) and NOT for naming files, test files and phases (that is ship-issue's plan step).
---

# Approach (GitKraken repos)

Read [../ship-issue/rules/host-runtime.md](../ship-issue/rules/host-runtime.md) first for host invocation, questions, planning, and fresh-agent dispatch. Workflow-internal references resolve from the containing document; repository-source pointers resolve from the target checkout. Profiles retain the conventions described in host-runtime.md.


Answer one question — **how should this be built, at altitude?** — and leave behind a
decision note good enough that planning starts from a settled direction and no later
session or review round re-litigates it. This skill ends at the decision. It plans
nothing, implements nothing, and does not re-derive the *what*.

Normally entered **from `/ship-issue` step 2**, which checks whether the approach is
contested and calls this skill when it is; an upstream verdict note never routes here
directly. Invoke it by hand only when the human wants direction settled before an
issue is filed — the one case where the choice changes the acceptance criteria.

Hard limits (the point of this skill vs. its neighbours):

- **No re-investigation.** The inputs are a verified problem (an `/investigate` note or
  a ship-issue step 1 note) or a stated feature intent (an `/explore` note, or the
  human in chat), plus acceptance criteria. If
  the problem is unverified, stop and say so — this skill would be choosing between
  ways of building the wrong thing. If no issue exists yet, take the intent and
  criteria from the human in chat and record them in the note; ship-issue step 1
  files the issue from them later.
- **No plan.** An option that names files, test files or phases has drifted into
  ship-issue step 2. A card names seams only at the level of "the event bridge", "a
  new backend setting", "the SWR cache".
- **No code changes.** Read-only on the repo, except a throwaway feasibility probe
  reverted before the session ends.

## 1. Frame

- Pull the inputs into context directly (`get_task_context` for the notes, GitKraken
  MCP for the issue). Restate in two lines what must be true when the work is done —
  the acceptance criteria are what every option is judged against and what settles
  scope disputes later.
- Read the **invariants source** section of the repo profile
  (`../ship-issue/profiles/<repo>.md`) — that section only; the rest is
  ship-issue's concern. Every option is weighed against these, and an option that
  touches one must say which.
- A direction the human named in the prompt becomes one option and gets the same card
  as the others — never a foregone conclusion, never omitted.

## 2. Enumerate

- **Dispatch the second-model option generation first**
  (`../ship-issue/rules/second-model-review.md`, option form), blind to
  anything below, and enumerate while it runs. Its options join the cards per that
  file's triage before you recommend.
- Produce **two or three credible options**. If only one is credible, and the second reviewer found
  no second one either, this skill has nothing to decide: record a one-line "single
  credible approach: <shape>" note so ship-issue step 2 can skip cleanly, and stop.
- **Every option needs a genuine advocate.** When the choice is contested — you cannot
  say in one sentence why one option wins — sketch each option with a separate
  fresh-context planning subagent, given only the frame from step 1 and the invariants,
  and asked to make the strongest case for its option *including* its costs. Sketching
  all options inline produces one real option and two strawmen.
- Survey **prior art** when a sibling client has solved the same capability
  (vscode-gitlens, gkcli, gitkraken-client, core-tools; first pass
  `gh search code --owner gitkraken`). Dispatch the survey to a fresh investigation
  subagent per sibling so only the distilled lesson returns; kepler, vscode-gitlens,
  gkcli and core-tools are registered in this workspace (`create_worktree`, discard
  when done), the rest read remotely via `gh` or the GitKraken MCP. A sibling's
  implementation is evidence for an option, not an option by itself — the lesson goes
  on the card, not a description of what the sibling does.
- **Card shape**, one per option, and the same shape for all (the option form in
  `second-model-review.md` asks the second reviewer for these fields; change them together):
  - *Shape*: one paragraph of how it works.
  - *Invariants touched* (from the profile's source) and any **new concept**
    introduced — an event, a table or migration, a setting, a flag, a wire route.
  - *Forecloses*: what it makes harder or impossible later.
  - *Size* (hours / days / multi-session) and the **main risk**.
  - *Hinges on*: the unknown that would flip the verdict, if any — this is what
    becomes a feasibility spike in step 3.
- A card running past ~10 lines is turning into a plan. Cut it back to altitude.
- **When the options differ in what the user sees or does**, show them on one `/design`
  canvas, one artboard per option, before asking
  (`../ship-issue/rules/design-canvas.md`). Each card names its artboard.

## 3. Recommend and decide

- Recommend one option, with the reason in two or three sentences. Then put the choice
  to the human with the host question mechanism — the recommendation first and labelled
  "(Recommended)", each option's description being its card's one-line trade-off. The
  human picks; "Other" is theirs.
- If the chosen option **hinges on an unknown**, name the spike: what to probe, the
  time box, and which result would flip the decision. The spike is Phase 0 of the plan,
  not something to run here — unless it is read-only and under ~15 minutes, in which
  case run it now and fold the result into the card before asking.
- If the human picks against the recommendation, record their reason if they give one
  and do not argue the pick. The decision is theirs; the note's job is to make it stick.

## 4. Decision note

Report the decision to the human in chat, and record it with `add_task_note` in ADR
shape — this is the interface ship-issue step 2 consumes. All seven parts, in order:

1. **Context**: the problem in one line, a link to the issue or the verification note,
   and the acceptance criteria being served.
2. **Options considered**: each option's one-line shape and **the reason it was not
   chosen**. The rejected options are the load-bearing part — they are what stops a
   later review round or session from re-proposing them.
3. **Decision**: the chosen option and why, in the human's words if they overrode the
   recommendation. If a `/design` canvas was used, its URL and the picked artboard.
4. **Consequences**: the invariants the plan must respect, the new concepts introduced,
   what is foreclosed, and the out-of-scope items that surfaced (these go to the plan's
   backlog, never into the MVP as defensive code).
5. **Spike**: the feasibility probe to run first, with its time box and flip condition,
   or "none".
6. **Baseline**: repo, commit sha, date — ship-issue's freshness check re-validates
   against this.
7. **Next step**: usually "invoke /ship-issue; approach decided — step 2 plans against
   this note", or "file the issue first (ship-issue step 1), then plan against this".

## Re-entry from a ship-issue review loop

When ship-issue step 5 oscillates and sends the work back here, the frame is unchanged
but the evidence is new: the failed shape becomes a **rejected option** with the
oscillation as its reason, and the round's findings are input to step 2 — they show
what the shape could not accommodate. Do not re-recommend the failed shape with a
guard added; a fix that adds a branch without deleting anything is the same shape.
