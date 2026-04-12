---
name: track-file
description: Track a new file in this dotfiles repo using the existing layout and Dotbot install flow
---

## When To Use

Use this skill when the user asks to add a new file under `$HOME` or an XDG path to this repo.

## Repo Layout

- `home/` maps to `$HOME`
- `xdg_config/` maps to `~/.config`
- `xdg_data/` maps to `~/.local/share`
- `xdg_bin/` maps to `~/.local/bin`
- `stateful/` holds sensitive or machine-tied files

## Workflow

1. Interpret the requested path as a path on disk.
2. Inspect the current repo structure and existing install config before editing.
3. Choose the smallest correct destination in the repo that matches the existing layout.
4. Copy the current file contents into the repo path without changing semantics.
5. Update the relevant Dotbot config under `install.d/base/` or a profile overlay with an explicit link if needed.
6. Reuse the existing install flow. Do not invent a new mechanism when a current mapping already fits.
7. Preserve unrelated worktree changes.
8. Verify with `./install --dry-run` when feasible and report the exact link action for the new file.

## Placement Rules

- Use `home/` for ordinary home-directory dotfiles such as `~/.ssh/allowed_signers`.
- Use `xdg_config/`, `xdg_data/`, and `xdg_bin/` for files that live in those destinations.
- Use `stateful/` for secrets, host-specific SSH config, and machine-tied files.
- If the correct destination is unclear, ask one short question before editing.

## Output

State:

1. Which repo path was added
2. Which install config was updated
3. Verification results or the blocker if verification could not run
