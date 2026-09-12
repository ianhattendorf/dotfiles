# Feature scale

When planning reveals the work spans multiple sessions or breaks into phases, the plan
is a **durable artifact, not a plan-mode plan**: write `PLAN.md` in the task folder,
record a pointer + approval status as a task note, and have the human review the file
itself.

## The plan must contain

- **Settled decisions** with the reason each was settled, so later sessions don't
  re-litigate. The approach decision note seeds this list: its chosen option and every
  rejected one, with reasons, are the first entries — along with any prior-art lessons
  its cards carry.
- **Phases with sizing**, each independently gateable, and the **demo-able milestone**
  named explicitly.
- **An out-of-scope backlog**, ordered, with known traps per item. Real-but-deferred
  concerns get parked here by name, never absorbed into the MVP as defensive code.
- **Risks / open items**, including decisions that belong to a human (product calls,
  copy, flag graduation) — flagged, not guessed.
- **PR strategy**: single PR at the end vs. one per phase (prefer per-phase when the
  feature is behind a flag and phases are independently green). The profile's changelog
  convention says how the single user-facing entry lands.

## The loop per phase

Steps 3–7 run once per phase, not once per feature. Each phase ends with gates green,
review converged, a checkpoint commit, and a task note recording phase status + what's
next. Run `/simplify` at each phase boundary — a multi-day diff simplified only at the
end is too late. The human live run (step 8) happens at the demo-able milestone AND
before the final PR. Review findings that are real but deferred go into the plan's
backlog with a note, same as rejected findings.
