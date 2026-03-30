# Dotfiles

Linux dotfiles, managed via [dotbot](https://github.com/anishathalye/dotbot). Currently using Fedora, i3wm, and fish.

NOTE: This will overwrite any existing dotfiles.

```sh
$ git clone https://github.com/ianhattendorf/dotfiles
$ cd dotfiles
$ ./install
```

To update dotbot:

```sh
$ cd dotfiles
$ git submodule update --init --remote
```

## Tmuxinator

This repo now manages tmuxinator projects in `~/.config/tmuxinator`.

`./install` will install tmuxinator with `gem install --user-install` if it is not already present, then you can start the included dotfiles workspace with:

```sh
$ tmuxinator start dotfiles
```

For a general-purpose session to open after signing in, use:

```sh
$ tmuxinator start default
```
