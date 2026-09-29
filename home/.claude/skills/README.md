# GitKraken development skills

Six skills cover one pipeline. They hand off through **durable task notes**, so each
one starts from a settled answer instead of re-deriving it, and any of them can be
resumed in a later session by reading the note.

## Which one

| You have | Skill | It ends at |
| --- | --- | --- |
| An idea, not a problem — what is it, and do I still want it? | `/explore` | A drop / park / commit note with drafted acceptance criteria. Files nothing. |
| A report, a hunch, a Sentry event — is this even real? | `/investigate` | A verdict note. Files nothing, fixes nothing. |
| `/ship-issue` step 2 (or an oscillating review loop) sent you here — the how is contested | `/approach` | A decision note with the rejected options and why. |
| Work you are committed to shipping | `/ship-issue` | An open PR. |
| An open PR of yours with feedback or red CI | `/address-feedback` | Every thread dispositioned, fixes pushed, replies drafted. |
| Someone else's PR, branch, commit or diff | `/review-change` | A verdict + findings handed to you. Changes nothing, posts nothing. |

## How they chain

```
/explore ──────┐
 verdict note  ├──→ /ship-issue ──→ /address-feedback
/investigate ──┘       the PR          each round
 verdict note            │ ▲
                    step 2 calls out when the how is contested
                         ▼ │
                      /approach
                    decision note
```

Every arrow is optional and every one is a note, not a conversation:

- `/ship-issue` step 1 accepts an `/investigate` note, or an `/explore` note with a
  `commit` verdict, as completed intake.
- An upstream note's next step is always `/ship-issue`, never `/approach`.
  `/ship-issue` step 2 plans against an `/approach` note; without one it decides
  whether the approach is contested, and runs `/approach` itself if it is. The
  upstream note records the fork it saw; it does not route on it.
- `/ship-issue` step 5 sends you *back* to `/approach` when review rounds oscillate —
  that means the fix shape is wrong, not that the fix needs another guard.
- `/review-change` is not in the chain. It reviews work you will not push to. Your own
  in-progress diff is reviewed by `/ship-issue`'s own rounds.

## Shared machinery

`ship-issue/` owns what the others reuse, so the rules have one home:

- `profiles/<repo>.md` — the per-repo facts every skill consumes (gates, invariants
  source, live verification, merge and changelog conventions, security-sensitive
  surfaces). **Resolve the profile first, in every skill.**
- `rules/fix-or-reject.md` — the review subagent, and what makes a finding fixable.
- `rules/comment-audit.md` — the comment bar and the cold-reader audit.
- `rules/freshness-check.md` — run on every resume, in every skill.
- `rules/design-canvas.md` — when and how /explore, /approach and /ship-issue use `/design`, and the handoff when it is unavailable.
- `feature-scale.md` — when the work outgrows a plan-mode plan.

## Two rules that hold everywhere

**The agent never speaks to humans on their behalf.** Replies to human review comments
are drafted and handed over, never posted. Issue and PR writes are confirmed in two
turns. Pushes need explicit confirmation every time.

**The dependency runs one way.** These skills read the repos; no repo file references
this directory. See `profiles/README.md` for why, and for what a repo skill does when
it needs something only an outer workflow can supply.
