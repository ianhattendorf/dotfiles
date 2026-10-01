# Host execution

Use the current host's section; do not infer the host from a repo's CLAUDE.md.
Workflow policy is shared. User instructions and the host's higher-priority rules
win, including authorization already given for the concrete action.

## Invocation and references

Claude invokes skills as `/name`; Codex uses `$name`. Workflow references using
`/name` mean the named procedure, not a shell command. Read its SKILL.md when the
host has no skill invocation tool. Resolve workflow-internal paths from the file containing the reference, including
through symlinked directories. Repository-source pointers (such as DESIGN.md,
docs/ARCHITECTURE.md, or src/) resolve from the target checkout. Existing profiles
retain their convention: repository pointers are checkout-relative and workflow
`rules/` pointers are relative to the ship-issue skill root. Resolve helper
paths to absolute paths before running them from the target repo.

## Questions and plans

Claude uses AskUserQuestion. Codex uses its available question tool only within
that tool's permitted scope and mode. An asynchronous question remains pending
until the user actually answers. If a required decision cannot use a question tool
(for example approval is disallowed by that tool), end the turn with a clear,
self-contained question and wait. No timeout, empty result, or proposed default
counts as approval. Optional unanswered preferences may use an explicitly stated
default only when the host permits it.

Keep drafts the human must review in a completed chat turn before requesting their
decision. Preserve the workflow's issue/PR draft and confirmation sequence. Do not
re-ask for authorization the user already supplied for that exact action.

Use plan mode when available, but never claim to have switched modes merely by
writing a plan. Otherwise write the plan, run its required independent review,
present it, and stop for approval before implementation. Record pending decisions
and the next step in task notes so another host can resume without guessing.

## Fresh agents

Give a new agent a bounded brief with primary sources, resolved diff or plan,
acceptance criteria, settled decisions, requested output, and allowed side effects.
Do not fork the writer's transcript into a reviewer. Reviews and comment audits
are read-only; implementation stays with the parent.

Claude uses Agent: investigation/general work uses general-purpose, planning uses
Plan. For correctness reviews, use the restricted Opus CLI procedure unless a
custom Opus agent has an enforced read-only tool allowlist. Codex uses its available fresh-agent tool
(for example spawn_agent with fork_turns="none") and the current session model
unless the review rule explicitly selects a model. Never send `opus`, `sonnet`, or
Claude subagent types to a Codex tool. Use only supported effort settings; if the
required effort cannot be expressed, report the limitation before treating the
gate as met. Each reviewer receives the relevant review rule and effort floor.

If the fresh-agent tool cannot restrict a reviewer to read-only access, use the
same-family CLI procedure in `second-model-review.md` with the primary review
brief. The separate second-model review must still use the other family. For
second-model dispatch, always follow that file's explicit Astra/Opus selections.
Never replace required fresh review with an inline self-review.

## Capabilities and live work

Before a step, check its required tools: task context/notes, GitKraken issue/PR
writes, source access (including Slack when needed), and browser/live inspection.
Use tools actually exposed by this host, not guessed MCP names. A missing required
capability blocks that step; report what is unavailable and preserve a resume note.
Do not fall back to gh writes when the workflow requires GitKraken MCP writes.

Repo-owned live skills may describe Claude tool names or a serve-driver agent.
Read that agent's instructions and dispatch an equivalent fresh Codex agent when
available, carrying the same probes and ownership rules. If required browser tools
are missing, arrange a supported handoff; do not count static inspection as a live
pass. Assign each concurrent driver a distinct KEPLER_INSTANCE, record the worktree
and slot, and stop only that slot with `pnpm serve:isolate stop` under the same env.
Do not terminate processes by name or worktree pattern.
