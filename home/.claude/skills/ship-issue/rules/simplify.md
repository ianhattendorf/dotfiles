# Simplify once after correctness converges

Review the cumulative diff for four cleanup concerns: reuse of existing helpers,
unnecessary complexity or redundant state, avoidable work/allocation, and code at
the wrong level of abstraction. Read surrounding implementations before suggesting
reuse. Return file/line, the concrete maintenance or runtime cost, and a scoped fix.

Claude may invoke its native `/simplify` with this scope. Codex dispatches fresh
read-only cleanup reviewers through `host-runtime.md`, one per concern, within the
host's concurrency limit. Neither route expands the change into unrelated cleanup.

The parent deduplicates findings and applies only fixes that preserve intended
behavior and stay within the reviewed scope. Record why a suggestion is skipped.
Do not manufacture an abstraction for a single use or add speculative safeguards.
Correctness defects go through `fix-or-reject.md`, not a separate unreviewed fix loop.

Run once at the calling step or phase boundary. Re-run the profile gates; any code
change returns to correctness review and its convergence rule. Do not repeat this
pass inside each correctness round.
