#!/usr/bin/env python3
"""Print the Codex model every second-model dispatch passes as `-m`.

Exit 0: stdout is the slug.
Exit 2: the pinned model is no longer listed, or Codex offers an upgrade path for it.
Exit 3: Codex lists a model that was not listed when the pin was last chosen.
"""
import json
import sys
from pathlib import Path

PIN = "gpt-6-astra"
KNOWN = {
    "gpt-6-sol",
    "gpt-6-astra",
    "gpt-6-luna",
    "gpt-5.6-sol",
    "gpt-5.6-terra",
    "gpt-5.6-luna",
    "gpt-5.5",
}

cache = Path.home() / ".codex" / "models_cache.json"
try:
    models = {m["slug"]: m for m in json.loads(cache.read_text())["models"]}
except (OSError, ValueError, KeyError) as e:
    print(f"warning: cannot read {cache} ({e}); {PIN} is unchecked", file=sys.stderr)
    print(PIN)
    sys.exit(0)

listed = {slug: m for slug, m in models.items() if m.get("visibility") == "list"}


def show_listed():
    for slug, m in sorted(listed.items(), key=lambda kv: kv[1].get("priority", 99)):
        print(f"  {slug}: {m.get('description', '')}", file=sys.stderr)


pin = listed.get(PIN)
if pin is None or pin.get("upgrade"):
    upgrade = (pin or {}).get("upgrade") or {}
    reason = "is no longer listed" if pin is None else f"has an upgrade to {upgrade.get('model')}"
    print(f"stale: {PIN} {reason}", file=sys.stderr)
    if upgrade.get("migration_markdown"):
        print(f"  {upgrade['migration_markdown']}", file=sys.stderr)
    print("listed now:", file=sys.stderr)
    show_listed()
    sys.exit(2)

new = sorted(set(listed) - KNOWN)
if new:
    print(f"new since the pin was chosen: {', '.join(new)}", file=sys.stderr)
    print("listed now:", file=sys.stderr)
    show_listed()
    sys.exit(3)

print(PIN)
