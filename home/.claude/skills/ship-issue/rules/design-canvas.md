# Design canvas (`/design`) — shared by /explore, /approach, /ship-issue

`/design <brief>` is Claude Code's artboard skill: it publishes a canvas of editable UI
options to claude.ai as an artifact, the human picks and tweaks one, and the pick becomes
the visual target for the implementation. It is a way of *looking at* a UI question, so
it belongs wherever a UI question is still open, and nowhere else.

## When

- **/explore**: the discussion is stuck on "would this even feel right" for a UI idea.
- **/approach**: the options differ in what the user sees or does, not only in seams.
  One artboard per option.
- **/ship-issue step 2**: a UI change whose visual shape no earlier note settled, and
  small enough that /approach is not warranted.

Not for backend work, copy-only tweaks, or a change whose shape the repo's existing
components already dictate. It is token-hungry: one canvas per open question, not one
per iteration. Refine on the canvas, not by re-running.

## How

- **The model cannot run it.** In this mode `/design` is human-invocable only, so draft
  the brief and hand the human one line to type, in a fenced block so it copies
  verbatim. Do not imitate it through the Artifact tool directly.
- **The brief is the skill's contribution**, so ground it: the surface and where it
  sits, what exists there today (from the grounding), the options or constraints
  already settled, the number of artboards, and a pointer to the repo's design system
  (the profile names it).
- When the human reports back, record the **canvas URL and the picked artboard** in the
  skill's own note: /explore under *Grounding*, /approach on the option cards and in
  *Decision*, /ship-issue in the plan as the visual target.
- **The artboard is a mock.** The repo's design system wins wherever they disagree;
  implementing an artboard never introduces a hardcoded value.

## When it is not available

It is available when `/design` is described as "Make a new Design artifact from a brief".
If it is described as "Grant or revoke Claude agent access to your Design projects", or
answers that it only manages Claude Design access, artifacts are off in this session.
Usual causes: a Kepler ACP session on a build without the artifact opt-in, an API-key or
cloud-provider session, or organization policy.

Say which in one line, then offer:

1. **Kepler ACP, fix in place**: add `"env": { "CLAUDE_CODE_ARTIFACT": "1" }` to
   `~/.claude/settings.json` and start a new session. A human's call, since it
   applies to every Agent SDK session on the machine.
2. **Terminal handoff**: the brief is self-contained, so a fresh terminal session needs
   nothing else:

   ```
   cd <worktree path>
   claude
   /design <the brief>
   ```

   The human pastes the canvas URL and the pick back into this session, and the note is
   written here. Do not resume this session's conversation in the terminal while this
   one is live; two writers on one transcript diverge.
3. **Skip it**: fall back to the skill's non-visual path (a throwaway Storybook story in
   /explore, text option cards in /approach).
