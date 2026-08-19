#!/bin/sh
# Builds the op-less conflict fixture: greet.js left UU by a conflicted `git stash pop`,
# with no MERGE_HEAD or rebase state. The absence of a paused operation is the point —
# it's what exercises AI Resolve's conflict detection when there's nothing to "continue".
set -eu

dest=${1:-$HOME/dev/test/stash-conflict-repro}

rm -rf "$dest"
mkdir -p "$dest"
cd "$dest"

git init -q -b main
git config user.name 'Test Fixture'
git config user.email 'test@example.com'
git config commit.gpgsign false

cat > README.md <<'EOF'
# Stash-pop conflict repro

Repro for GitLens "No active merge, rebase, or cherry-pick conflicts to resolve." error.

`greet.js` is left `UU` by a conflicted `git stash pop`. There is no paused operation,
so anything that infers conflict state from MERGE_HEAD/rebase-merge sees nothing.

Rebuild from scratch with `stash-conflict.sh`, or re-conflict in place with:

```sh
git reset -q && git checkout -- greet.js && git stash apply
```
EOF

write_greet() {
	cat > greet.js <<EOF
function greet(name) {
  return \`$1\`;
}

module.exports = { greet };
EOF
}

write_greet 'Hello, ${name}'
git add -A
git commit -qm 'init'

# Stashed side: the work-in-progress the user set aside.
write_greet 'Hi there, ${name}!'
git stash -q

# Upstream side: the same line moved underneath the stash, so applying it conflicts.
write_greet 'Good day, ${name}.'
git commit -qam 'upstream change'

# Expected to exit 1 — the conflict IS the fixture, and the failed pop keeps the entry.
git stash pop >/dev/null 2>&1 || true

fail() { echo "stash-conflict: $1" >&2; exit 1; }

[ "$(git status --porcelain -- greet.js)" = 'UU greet.js' ] || fail 'greet.js is not UU'
[ -n "$(git stash list)" ] || fail 'stash entry was consumed'
[ ! -e .git/MERGE_HEAD ] || fail 'MERGE_HEAD present — no longer op-less'
[ ! -e .git/rebase-merge ] && [ ! -e .git/rebase-apply ] || fail 'rebase state present'

echo "stash-conflict: ready at $dest (greet.js UU, no paused operation)"
