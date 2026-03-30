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
DOTFILES_PROFILE=home ./install
DOTFILES_PROFILE=work ./install
DOTFILES_PROFILE=simba ./install
```

Supported profiles are `home`, `work`, `simba`, `nala`, `mba2020`, and `work-vm`.

There is no hostname-based install or runtime selection. If you need machine- or context-specific behavior, add it under `profiles/<name>/...` and install it explicitly with `DOTFILES_PROFILE=<name>`.

## Bootstrap Phases

`./install` runs three bootstrap phases through Dotbot shell steps:

- `bootstrap/profile-setup`
- `bootstrap/external-assets`
- `bootstrap/post-install`

`bootstrap/external-assets` installs external tools such as tmuxinator and fetches non-repo assets such as antigen and betterlockscreen.

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
