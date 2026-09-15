# Freshness check — on every resume

Tasks pause for days and `main` moves. Before continuing from the recorded step:

1. `git fetch origin`, then `git log --oneline $(git merge-base HEAD origin/main)..origin/main --
   <the files/seams the plan names and the branch touches>`, plus the linked issue's
   current state (closed? new comments? linked PRs?).
2. **Nothing relevant landed** → proceed from the recorded step.
3. **Relevant commits landed** → read them and classify:
   - **Premise gone** (the bug was fixed, the feature shipped another way): stop, report
     the evidence, and confirm with `AskUserQuestion` before acting — e.g. "Close the
     issue and abandon the branch" (recommended) / "Keep the branch, I want to look
     closer" / "Something else". Do not keep implementing to salvage sunk work while
     waiting on the answer.
   - **Plan invalidated** (the seams the plan names were refactored, an invariant
     changed): back to ship-issue step 2 for a plan revision with human approval.
   - **Mechanical overlap only**: rebase onto `origin/main` now, not at PR time —
     conflicts only grow with delay. Re-run gates after; a non-trivial conflict
     resolution is a fix and gets a review round.

## The premise can also die mid-step

The classification above is not only for a resume. When implementation itself shows the
premise is gone — the behavior turns out to be intended, the repro was environmental,
the acceptance criteria cannot be met as written — stop at that point and run the same
`AskUserQuestion` confirmation. Do not keep building to preserve the work already done,
and do not quietly reshape the issue to fit what the branch now does: the issue is the
agreement, and changing it is the human's call. Record the evidence in the task note
first, so the decision is made against it rather than against a summary.

Record the outcome in the running task note either way, so the next resume knows the
last-verified baseline. An `/investigate` or `/approach` note records the baseline sha
it was written against — check against that one.
