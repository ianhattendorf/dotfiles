# Comments — the bar and the audit

## The bar

A comment exists only to state what a competent reader cannot get from the code: a
constraint, an invariant, surprising behavior, a load-bearing why. "Not obvious" means
not derivable from the surrounding code. Never write comments that narrate the
mechanics, describe the change rather than the code ("now uses X", "previously Y"), or
argue the change's correctness to a reviewer — that belongs in the PR body. State a
fact once: if another comment in the diff (a sibling file, a parallel branch, an issue
number at every call site) already says it, merge or delete. Say it in one pass, as
briefly as it can be said — a comment edited across review rounds that reads like a
log of decisions gets rewritten to the current invariant, not appended to.

**When genuinely unsure whether a comment clears the bar, or how long it needs to be,
resolve toward no comment or the shorter one.** A missing comment costs a future
question; a verbose one costs every comment in the file its reader.

## The audit

Runs as a **fresh-context, read-only subagent**, never inline: the implementing session
wrote these comments and cannot un-know what they mean. Give it the diff, enough
surrounding file context to judge, and this file; it returns per-comment verdicts
(keep / delete / trim, one-line reason) and does not edit.

1. **Mechanical pass** per changed file: comment lines ÷ total lines
   (`grep -cE '^\s*(//|\*|/\*\*)'` against the line count is close enough). Over
   ~15–20% almost always means accreted or duplicated comments — read that file first
   and skeptically. This catches what per-comment judgment misses: several comments
   each true and non-obvious, restating one fact.
2. **Qualitative pass** against the bar above, same tie-break: when unsure, delete or
   trim, never keep.

Apply the verdicts in the main session. To keep a comment the cold reader flagged, name
the concrete trap it guards — and then **rewrite the comment so the trap is stated**; a
comment whose necessity is invisible to a cold reader has failed at its one job.
Comment-only edits: re-run the format gate and commit.
