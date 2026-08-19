#!/bin/sh
# Builds the automatic-rebase fixture (GitLens #5450): two scenarios in ONE repo, both
# rebasing onto main, so you switch branches rather than folders.
#
#   feature/multi-step-conflicts — 3 stops, all AI-resolvable, all in server.conf/docs.md.
#     Same-file edits, so a whole-file resolution collapses later steps; only hunk-level
#     resolution keeps all 3.
#   feature/ai-escalation        — 3 stops on 3 DISTINCT files so no step can collapse
#     another. Step 2 is binary (null bytes + `*.bin binary`), so extractConflict finds no
#     markers and the run escalates to the Resolve panel instead of resolving with AI.
set -eu

dest=${1:-$HOME/dev/test/rebase-multi-conflict}

rm -rf "$dest"
mkdir -p "$dest"
cd "$dest"

git init -q -b main
git config user.name 'Test Fixture'
git config user.email 'test@example.com'
git config commit.gpgsign false

server_conf() {
	cat > server.conf <<EOF
# Sample service configuration

host = localhost
port = $1

timeout = $2
retries = 3

loglevel = $3
maxconns = 50
EOF
}

docs_md() {
	cat > docs.md <<EOF
# Service Docs

The service listens on the default port below.

Default port: $1

See server.conf for full configuration.
EOF
}

config_yaml() {
	cat > config.yaml <<EOF
service:
  name: demo
  replicas: $1
  region: $2
EOF
}

# 12 bytes with embedded NULs so git sees binary regardless of .gitattributes.
icon_bin() { printf "$1" > assets/icon.bin; }

ICON_BASE='\211BIN\000\001\002\003\004\000\377\020'
ICON_MAIN='\211BIN\000\020\040\060\100\000\376\021'
ICON_FEAT='\211BIN\000\252\273\314\335\000\001\022'

# --- base ---------------------------------------------------------------------

cat > README.md <<'EOF'
# rebase-multi-conflict

Two deterministic fixtures for GitLens automatic rebase (#5450). Both feature
branches rebase onto `main`; switch branches (same folder) to pick a scenario.

## Scenario A — `feature/multi-step-conflicts` (all AI-resolvable)

Three text commits over `server.conf` / `docs.md` that `main` also changed.
`git checkout feature/multi-step-conflicts && git rebase main` stops 3 times, all
resolvable by AI. (Note: these edits are in the SAME file, so a resolution that
takes the feature side wholesale can collapse later steps.)

| Step | Feature commit           | Conflicting file(s)      |
| ---- | ------------------------ | ------------------------ |
| 1    | Use dev port 3000        | `server.conf`, `docs.md` |
| 2    | Extend timeout for tests | `server.conf`            |
| 3    | Trace-level logging      | `server.conf`            |

## Scenario B — `feature/ai-escalation` (mid-run human handoff)

Three commits over THREE distinct files, so the steps are fully independent —
none can collapse another. The middle step is a binary conflict with no markers,
which the AI cannot resolve and hands off to a human.

| Step | File              | Type   | Expected outcome                              |
| ---- | ----------------- | ------ | --------------------------------------------- |
| 1    | `service.py`      | text   | AI resolves                                   |
| 2    | `assets/icon.bin` | binary | **Escalates** — no markers, hand off to human |
| 3    | `config.yaml`     | text   | AI resolves (after taking over the rest)      |

## Reset anchors (tags)

- `main-orig` — full `main` tip
- `feature-orig` — `feature/multi-step-conflicts` tip
- `escalation-orig` — `feature/ai-escalation` tip
- `base` — scenario A ancestor · `esc-base` — scenario B ancestor

```sh
git rebase --abort 2>/dev/null || true
git branch -f main main-orig
git checkout -f feature/multi-step-conflicts && git reset --hard feature-orig
git checkout -f feature/ai-escalation       && git reset --hard escalation-orig
```

Or rebuild from scratch with `rebase-multi-conflict.sh`.
EOF

server_conf 8080 30 info
docs_md 8080
git add -A
git commit -qm 'Initial service config'
git tag base

# --- main: scenario A's upstream side -----------------------------------------

server_conf 9090 30 info
docs_md 9090
git commit -qam 'Point service at prod port 9090'

server_conf 9090 60 info
git commit -qam 'Raise default timeout to 60s'

server_conf 9090 60 debug
git commit -qam 'Enable debug logging'

# --- main: scenario B's files land, then its upstream side --------------------

mkdir -p assets
cat > .gitattributes <<'EOF'
# Force binary merge semantics so a conflict has no text markers (belt-and-suspenders
# alongside the file's null bytes) — this is what makes the AI escalate on *.bin.
*.bin binary
EOF

cat > service.py <<'EOF'
def handler(request):
    # Handle an incoming request.
    validate(request)
    return respond(request, status=200)
EOF

config_yaml 1 us-east-1
icon_bin "$ICON_BASE"
git add -A
git commit -qm 'Add project files for AI-escalation scenario'
git tag esc-base

cat > service.py <<'EOF'
def handler(request):
    # Handle an incoming request (main: enforce authorization).
    validate(request)
    authorize(request)
    return respond(request, status=200)
EOF
git commit -qam 'Enforce authorization in handler'

icon_bin "$ICON_MAIN"
git commit -qam 'Refresh icon asset'

config_yaml 3 us-west-2
git commit -qam 'Scale to 3 replicas in us-west-2'
git tag main-orig

# --- scenario A branch --------------------------------------------------------

git checkout -q -b feature/multi-step-conflicts base

server_conf 3000 30 info
docs_md 3000
git commit -qam 'Use dev port 3000'

server_conf 3000 120 info
git commit -qam 'Extend timeout to 120s for slow tests'

server_conf 3000 120 trace
git commit -qam 'Trace-level logging for debugging'
git tag feature-orig

# --- scenario B branch --------------------------------------------------------

git checkout -q -b feature/ai-escalation esc-base

cat > service.py <<'EOF'
def handler(request):
    # Handle an incoming request (feature: apply rate limiting).
    validate(request)
    rate_limit(request)
    return respond(request, status=429)
EOF
git commit -qam 'Add rate limiting to handler'

icon_bin "$ICON_FEAT"
git commit -qam 'Restyle icon asset'

config_yaml 5 eu-central-1
git commit -qam 'Scale to 5 replicas in eu-central-1'
git tag escalation-orig

git checkout -q main

# --- verify -------------------------------------------------------------------

fail() { echo "rebase-multi-conflict: $1" >&2; exit 1; }

for t in base esc-base main-orig feature-orig escalation-orig; do
	git rev-parse -q --verify "refs/tags/$t" >/dev/null || fail "missing tag $t"
done

[ "$(git rev-list --count base..feature-orig)" = 3 ] || fail 'scenario A is not 3 commits'
[ "$(git rev-list --count esc-base..escalation-orig)" = 3 ] || fail 'scenario B is not 3 commits'
git check-attr binary -- assets/icon.bin | grep -q 'binary: set' || fail 'icon.bin not marked binary'

# Rebase for real in a throwaway worktree — the only way to prove the conflicts exist.
# Worktrees keep their own rebase state, so this can't disturb the fixture itself.
verify_first_stop() {
	branch=$1
	expected=$2
	wt=$(mktemp -d)/wt

	git worktree add -q --detach "$wt" "$branch"
	(cd "$wt" && git rebase main >/dev/null 2>&1) || true
	actual=$(cd "$wt" && git diff --name-only --diff-filter=U | sort | tr '\n' ' ')
	(cd "$wt" && git rebase --abort >/dev/null 2>&1) || true
	git worktree remove --force "$wt"
	rmdir "$(dirname "$wt")" 2>/dev/null || true

	[ "$actual" = "$expected " ] || fail "$branch first stop was '$actual', expected '$expected'"
}

verify_first_stop feature/multi-step-conflicts 'docs.md server.conf'
verify_first_stop feature/ai-escalation 'service.py'

echo "rebase-multi-conflict: ready at $dest (2 scenarios, 3 stops each, on main)"
