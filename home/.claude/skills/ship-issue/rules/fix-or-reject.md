# Review findings — the subagent and the fix-or-reject rules

## The review subagent

Dispatch a fresh `Agent` subagent to review the diff — never review inline. The session
that wrote the diff shares its blind spots, and `/code-review` runs in the calling
session rather than a subagent, so don't invoke it directly. `model: "opus"`, effort at
least the floor the calling skill names, self-escalating to `high`/`xhigh` when the
subagent judges the diff warrants deeper coverage — never below the floor on a
self-judged call.

**Every round reviews the CUMULATIVE diff** (`merge-base(origin/main)..HEAD`), never
only what changed since the last round. A round-3 fix interacting badly with round-1
code is invisible to an incremental read, and that interaction is exactly what the
oscillation rule exists to catch — an incremental review cannot see the oscillation it
is supposed to report. The subagent is fresh each round, so it costs nothing to re-read
what it has never seen.

**The last round before a push runs at floor `high`,** whatever floor the calling skill
names for its inner rounds. The pre-PR loop is the argument for not requesting bot
reviewers, and a human reviewer is not guaranteed — so the final read is the one that
may be the only review the change ever gets, and it gets the same floor `/review-change`
uses for that reason.

Have it reason explicitly about interleaving: for every new trigger into existing
shared or async state (a cache, lock, in-flight guard, event subscription), what
happens if it fires while another trigger on the same state is mid-flight.

Two independent sources correlated only by an id (an RPC response and a broadcast
event; a streamed chunk and an exit signal) can arrive in either order, and a listener
gated on a not-yet-known id can race the event meant to fill it. A static diff read
misses this because the code looks correct in sequence. When a change adds a new pair
of independent async sources, walk every interleaving, and name any gap that could not
be exercised as an UNVERIFIED risk in the PR body rather than a generic "couldn't test
it".

## Fix or reject

Applies to every finding, whether from the subagent or a PR reviewer. A finding is
not an order.

- Fix only findings with a **concrete failure scenario**: specific input or state
  leading to a specific wrong output. "Could theoretically" is not a scenario.
- An ask that enforces a repo convention or a profile invariant is fixable without a
  scenario. Pure preference with neither is a reject — with a rationale, never silence.
- A fix that adds a branch without deleting anything is suspect by default — prefer a
  fix that restructures over one that guards.
- A fix that touches an existing comment rewrites it to state the current invariant
  in one pass (`comment-audit.md`), never appends a second justification.
- Every rejected finding gets a one-line rationale in a task note, so later rounds and
  sessions do not re-raise it. Check that note before triaging a new round.
- Out-of-scope findings (real but unrelated) are not fixed here — note them, and file
  or name them in the PR description.
