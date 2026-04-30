# Dotfiles

Linux-first dotfiles managed with [Dotbot](https://github.com/anishathalye/dotbot), organized around install destinations and explicit install profiles.

## Layout

- `home/` maps to `$HOME`
- `xdg_config/` maps to `~/.config`
- `xdg_data/` maps to `~/.local/share`
- `xdg_bin/` maps to `~/.local/bin`
- `stateful/` holds tracked sensitive or machine-tied files
- `profiles/` contains explicit install overlays
- `bootstrap/` contains install helper scripts
- `vendor/dotbot/` is the Dotbot submodule

## Install

Base install:

```sh
./install
```

Profile install:

```sh
DOTFILES_PROFILE=personal ./install
DOTFILES_PROFILE=work ./install
DOTFILES_PROFILE=simba ./install
```

Supported profiles are `personal`, `work`, `work-mac`, `simba`, `nala`, `mba2020`, and `work-vm`.

There is no hostname-based install or runtime selection. If you need machine- or context-specific behavior, add it under `profiles/<name>/...` and install it explicitly with `DOTFILES_PROFILE=<name>`.

## Profile Inheritance

A profile YAML may declare a parent on its first line as a YAML comment:

```yaml
# parent: work
- link:
    ...
```

The installer resolves the chain (oldest ancestor first, child last), appends each YAML to the Dotbot config in order, and lets the child override any duplicate target via Dotbot's `force: true` default. `work-mac` inherits from `work` so all OS-portable work content lives in the `work` profile and only macOS-specific overrides (signing key path, brew tmux path, Alt-arrow word-motion bindings) live in `work-mac`.

## Bootstrap Phases

`./install` runs three bootstrap phases through Dotbot shell steps:

- `bootstrap/profile-setup`
- `bootstrap/external-assets`
- `bootstrap/post-install`

`bootstrap/external-assets` installs external tools such as tmuxinator, fetches non-repo assets such as Antidote and betterlockscreen, and pins tmux plugins to fixed upstream commits.

## Tmuxinator

Tmuxinator projects are managed under `xdg_config/tmuxinator` and install to `~/.config/tmuxinator`.

After install:

```sh
tmuxinator start dotfiles
```

For a general-purpose session to open after signing in, use:

```sh
$ tmuxinator start default
```

## Tmux Plugins

Tmux plugins are cloned directly into `~/.tmux/plugins` and checked out to fixed commits during bootstrap.

Configured plugins:

- `tmux-resurrect` at `cff343cf9e81983d3da0c8562b01616f12e8d548`
- `tmux-continuum` at `0698e8f4b17d6454c71bf5212895ec055c578da0`

## Simba TPM SSH Agent

The `simba` profile uses `ssh-tpm-agent` as `SSH_AUTH_SOCK` at `$XDG_RUNTIME_DIR/ssh-tpm-agent.sock`.
The profile install links the systemd user units and enables `ssh-tpm-agent.socket`.

Create or inspect TPM-backed SSH keys locally on Simba:

```sh
ssh-tpm-keygen
ssh-tpm-keygen --print-pubkey ~/.ssh/id_ecdsa.tpm
```

TPM sealed private key files are machine-bound and should remain local; do not add `.tpm` keys to this repo.
