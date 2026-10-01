# Review findings — the subagent and the fix-or-reject rules

## The review subagent

Dispatch a fresh, read-only reviewer through `host-runtime.md` — never review
inline or give it the writer's transcript. Claude uses an Opus subagent; Codex uses
its supported fresh-agent interface with the session model. Use at least the effort
floor the calling step names, escalating to high/xhigh when needed and supported.
If the host cannot enforce read-only review, use the restricted CLI route for that
family in `second-model-review.md`; inability to dispatch is a blocked review.

**Every round reviews the CUMULATIVE diff** — the full diff the calling skill resolved
for its target (ship-issue's own branch: `merge-base(origin/main)..HEAD`), never
only what changed since the last round. A round-3 fix interacting badly with round-1
code is invisible to an incremental read, and that interaction is exactly what the
oscillation rule exists to catch — an incremental review cannot see the oscillation it
is supposed to report. The subagent is fresh each round, so it costs nothing to re-read
what it has never seen.

**The last round before a push runs at floor `high`,** whatever floor the calling skill
names for its inner rounds. The pre-PR loop is the argument for not requesting bot
reviewers, and a human reviewer is not guaranteed — so the final read is the one that
may be the only review the change ever gets, and it gets the same floor `/review-change`
uses for that reason. **That round also runs the second-model review**
(`second-model-review.md`, effort `high`) and merges its findings with the subagent's
before triage — a different model family is the only reviewer that does not share the
writing family's blind spots.

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

### What the dispatch prompt carries

A review can only find a gap its prompt leaves visible. kepler#3127 ran six Claude
rounds and three Codex runs, and a single `/review-change` afterwards still found one
blocker and ten nits. Each rule below comes from something those prompts did.

- **Primary sources, never a summary of them.** Point the reviewer at the contract
  itself: the tool's own description, the bridge or SDK code, the API schema, the issue.
  Have it list the input and response shapes from that source. A hand-written list
  ("publish returns `{url, title, …}`") reviews the code against the summary, so a case
  the summary dropped can never surface. In that PR the dropped cases were single-asset
  delete, asset copy, and the icon a republish omits.
- **The settled list holds decisions, not facts.** A human's product call ("chip
  provenance is the first publisher") may be marked do-not-re-raise. A factual claim
  ("a history replay carries publishes Kepler never saw") goes in only with its evidence
  (a `file:line`, a probe, a test), and the reviewer is told it may challenge it. In
  that PR an unverified premise sat on the settled list for five rounds. The same claim
  was in a code comment, and a later reviewer built a false P2 on the comment.
- **Standing checks, every round, besides interleaving:**
  - **Agent-authored text is untrusted input.** Model-chosen free text (a tool's icon
    word, a title, an id) can reach a lookup, a render or a key. Check what it does with
    `__proto__`, `constructor`, an empty string, a huge string or markup.
  - **Consumers outside the diff.** For every new kind, enum member, classification or
    branch order, grep every existing reader and switch that dispatches on it, not only
    the files the diff touches. Examples: a classifier that checks `subagent-child`
    first, a control gated on "has links".
- **Scope is the whole diff.** "Focus hardest on the newest commit" is allowed only on
  top of the cumulative read, never instead of it. A length cap on the report is fine;
  a cap that makes the reviewer drop areas is not. Say which areas it may summarise as
  clean.
- **It may prove a suspicion by running it**: a scratch test or script in the session
  scratchpad, never in the repo, deleted after. A rendered crash or a reproduced stale
  row beats a read-only argument, and it arrives already verified. It never edits
  tracked files.

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
  sessions do not re-raise it. Check that note before triaging a new round. A rationale
  that rests on a fact about the system cites the evidence that proves it (checked now,
  not recalled). Without evidence it is an open question, not a settled rejection.
- Out-of-scope findings (real but unrelated) are not fixed here — note them, and file
  or name them in the PR description.
