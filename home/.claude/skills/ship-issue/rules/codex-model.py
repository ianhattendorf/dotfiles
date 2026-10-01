#!/usr/bin/env python3
"""Print the user-selected Astra model; exit 2 when a readable catalog lacks it."""
import json
import os
import sys
from pathlib import Path

PIN = "gpt-6-astra"


def select_model(cache):
    try:
        models = json.loads(cache.read_text())["models"]
        slugs = {model["slug"] for model in models}
    except (OSError, ValueError, KeyError, TypeError) as error:
        print(f"warning: cannot read {cache} ({error}); {PIN} is unchecked", file=sys.stderr)
        return PIN
    if PIN not in slugs:
        print(f"unavailable: {PIN} is not in {cache}; no fallback selected", file=sys.stderr)
        return None
    return PIN


def main():
    config_dir = Path(os.environ.get("CODEX_HOME") or Path.home() / ".codex")
    model = select_model(config_dir / "models_cache.json")
    if model is None:
        return 2
    print(model)
    return 0


if __name__ == "__main__":
    sys.exit(main())
