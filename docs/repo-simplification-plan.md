# Simplify and Unify Dotfiles Structure

## Summary

Restructure the repo to an XDG-first, profile-aware layout with one canonical source tree for managed files, a small bootstrap layer, and a declarative install model. Keep Dotbot as the installer for now, but isolate vendor/bootstrap concerns from dotfile content. Remove hostname-driven scripts entirely, replace them with explicit profile overlays, and eliminate path-by-path sprawl in the installer.

## Key Changes

- Adopt this canonical repo shape:
  - `home/` for files that install directly into `$HOME`
  - `xdg_config/` for `~/.config/...`
  - `xdg_data/` for `~/.local/share/...`
  - `xdg_bin/` for `~/.local/bin/...`
  - `stateful/` for machine-tied or sensitive managed files that need explicit review
  - `profiles/` for named overlays such as `home`, `work`, `simba`, `nala`, `mba2020`
  - `bootstrap/` for install helper scripts
  - `vendor/dotbot/` for the submodule
- Replace the current mixed source-of-truth model:
  - Move root-managed directories like `.emacs.d`, `.local`, `.siril`, `.ssh`, `.vim`, `.gnupg` under the canonical source tree.
  - Keep installed destinations unchanged unless a change is required to fix a structural inconsistency.
- Remove hostname scripts and hostname-based behavior:
  - Delete host-specific scripts like `config/i3/scripts/nala.sh`, `config/i3/scripts/simba.sh`, `config/i3/scripts/work-vm.sh`, and host-detect helpers like `mpv-hwdec.sh` and the hostname branch inside `gitconfig-local.sh`.
  - Replace all hostname-conditional behavior with explicit profile overlays selected at install time.
  - Model machine/context variants as `profiles/<name>/...` files or fragments rather than suffix files like `ianh-dell.conf.home` and `ianh-dell.conf.work`.
- Make profiles first-class:
  - Support `DOTFILES_PROFILE=<name> ./install` as the only profile selection mechanism.
  - Merge a base layer plus one selected profile during install; do not fall back to hostname detection.
  - Ensure profile-specific files are obvious from path and naming alone.
- Simplify install orchestration:
  - Break `install.conf.yaml` into a small base config plus grouped install definitions by destination class.
  - Replace the long `mkdir -p` and `clean` lists with directory-level links where safe, and file-level links only where unmanaged files are likely.
  - Move shell side effects into named bootstrap steps: `bootstrap/external-assets`, `bootstrap/profile-setup`, `bootstrap/post-install`.
- Normalize subsystem organization:
  - Group shell, editor, WM/desktop, terminal, media, and chat tooling consistently inside the destination-based layout.
  - Treat `tmuxinator` as normal managed config under `xdg_config/tmuxinator`, not a special-case install feature.
  - Separate shared WM config from profile overlays so base `sway`/`i3` behavior is common and profile files only define outputs or machine-specific commands.
- Update docs:
  - Write the plan to `docs/repo-simplification-plan.md`.
  - Rewrite README around the new layout, explicit profiles, and bootstrap phases.

## Public Interfaces / Behavior

- `./install` remains the entrypoint.
- `DOTFILES_PROFILE=<name> ./install` is required for non-default overlays.
- No runtime or install-time hostname detection remains in managed scripts.
- New configs must be added under `home/`, `xdg_config/`, `xdg_data/`, or `xdg_bin/`; no new top-level dotfile source directories.
- Machine-specific variants must live under `profiles/`, not filename suffixes or host-check shell scripts.

## Test Plan

- Validate install behavior in a clean temp home for:
  - base install
  - `DOTFILES_PROFILE=home`
  - `DOTFILES_PROFILE=work`
  - one machine-oriented profile such as `simba`
- Verify no broken installer references remain, especially around sway profile files and directory links.
- Verify directory-level linking does not trample unmanaged files in mixed directories like `~/.config/Code/User` or `~/.config/weechat`.
- Verify bootstrap steps are idempotent across repeated installs.
- Smoke-check runtime-critical areas:
  - shell startup
  - sway/i3 profile application
  - tmuxinator project availability
  - mpv config behavior after removing host detection
  - git local include selection after removing hostname logic
  - systemd user reload and desktop database update

## Assumptions and Defaults

- Keep Dotbot rather than replacing the installer in this pass.
- Preserve installed target paths unless a path change is needed to correct a structural bug.
- Treat this as a Linux-first repo.
- Do not broaden secrets handling in this pass; only reorganize already tracked sensitive files.
- The plan file target is `docs/repo-simplification-plan.md`.
