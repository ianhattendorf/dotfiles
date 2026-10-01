# Focused security review

Run when the calling workflow identifies a security-sensitive surface or dependency
change. Resolve the exact cumulative diff and read the profile's security surfaces.
Dispatch a fresh read-only reviewer through `host-runtime.md`. Claude may use its
native `/security-review` inside that fresh context if it preserves this scope.

Trace changed trust boundaries from attacker-controlled inputs to privileged
operations: authentication/authorization, injection and execution, file access,
secret exposure, unsafe parsing, and dependency changes. Inspect existing guards
and actual callers before claiming a bypass. For dependency findings, verify the
changed version and reachable usage against authoritative advisory/source evidence;
do not infer a vulnerability merely from a version change.

Return only introduced or worsened issues with a concrete attack scenario,
preconditions, source evidence, impact, and confidence. Filter false positives and
pre-existing defects. Mark inaccessible evidence as unverified; do not label a
failed review clean. Never execute an exploit against a live external service.

Triage through `fix-or-reject.md`. The parent applies accepted fixes, runs gates,
and returns through correctness convergence. The reviewer changes and posts nothing.
