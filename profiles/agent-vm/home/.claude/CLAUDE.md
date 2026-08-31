# Global instructions

## Tooling

- The `gh` CLI's personal access token is read-only (no `pull_request`/`issues` write scope) — `gh pr create`, `gh issue create`/`comment`, etc. fail with "Resource not accessible by personal access token". Use the GitKraken MCP tools instead (`mcp__GitKraken__pull_request_create`, `issues_create`, `issues_add_comment`, ...) for any GitHub write operation. `gh` read commands (`gh pr view`, `gh api` GETs, etc.) still work fine.
- When the *ideal* write is one the tooling can't perform, hand it to the human — do NOT substitute a different write just because it happens to be available. Concrete case: updating a PR body is not possible (`gh` token is read-only; the GitKraken MCP can create PRs but not edit them) while PR *comments* are possible — if editing the body is the right approach, paste the exact intended body text in chat for the human to apply, rather than leaving a comment as a workaround. Paste it copyable, not rendered: wrap the whole text in a fenced code block (use `````markdown` with a longer fence than any fence inside it) so the raw markdown can be copied exactly — prose outside a fence gets rendered by the terminal and can't be copied verbatim. The available-but-wrong channel is not a fallback for the right-but-unavailable one; the human is.

## Commenting rules

- Only comment non-obvious business rules, architectural trade-offs, or workarounds for third-party bugs. Code should be self-documenting otherwise.
- When a fix touches code that already has an explanatory comment, rewrite the comment in one pass to state the *current* behavior/invariant — do not append a new justification on top of the old one. A comment that has been edited across multiple rounds of fixes and still reads like a log of decisions is a sign it needs a rewrite, not another addition.
- One comment should convey one non-obvious fact. If it explains what the code does, why it does it, *and* which invariant it protects, that's a sign to cut two of the three.
