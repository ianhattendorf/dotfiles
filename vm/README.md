# agent-vm

A contained Fedora guest for coding agents — Claude Code, codex, and kepler-driven tasks.

The VM is the **containment boundary**: whatever an agent does stays in the guest, never
the host. That is its only job. Unlike the earlier claude-vm design there is **no
rollback** — the guest runs continuously, hosts several kepler tasks at once in
kepler-managed worktrees, and stays current (OS auto-updates, Claude self-updates, the
rest refreshes on re-provision). Recovery is destroy + recreate from `provision.sh`,
which is kept fully reproducible for exactly that reason.

## Command reference

| Command | Does |
| --- | --- |
| `bootstrap` | One-time host setup: image dir, SSH key, **egress rules** |
| `create` | Build the guest from nothing: base image, cloud-init, seed kepler, provision, autostart |
| `provision` | Re-run provisioning against the live guest (idempotent; also repairs) |
| `seed <path> [name]` | Push a host repo into the guest — how **private** repos get in |
| `push [--repo name] [branch]` | Send the host's current HEAD to a guest checkout |
| `fetch [--repo name] <branch>` | Bring a guest branch back as `vm/<branch>` |
| `claude [args…]` | Claude Code in tmux inside the guest (`--permission-mode auto`) |
| `codex [args…]` | codex in tmux inside the guest (pass its own flags as args) |
| `fixtures` | Rebuild the scenario repos under `~/dev/test` |
| `egress` | Reapply the host-side nftables rules **and** the firewalld zone binding |
| `ssh [cmd…]` | Shell in (or run a one-off command) |
| `logs` | Read the guest journal straight off the disk — works with no network |
| `rescue` | Root shell on the guest's disk (guest must be stopped) |
| `start` / `stop` / `status` | Lifecycle; `status` includes egress and firewall state |
| `destroy` | Delete domain + system disk; state disk only with a second confirmation |

First run: `bootstrap` → `create` → log into the agents (`agent-vm claude`,
`agent-vm codex`). Update flows: `seed` again to refresh the kepler repo from the host;
`provision` again to refresh everything else.

## One-time host setup

```sh
sudo dnf install -y @virtualization guestfs-tools
sudo usermod -aG libvirt "$USER"        # re-login afterwards

# Fedora presets the MODULAR daemons. Do NOT `enable --now libvirtd` — it's preset-disabled
# on Fedora and conflicts with these.
sudo systemctl enable --now virtqemud.socket virtnetworkd.socket virtstoraged.socket

virsh -c qemu:///system net-list        # 'default' must be active
agent-vm bootstrap                      # image dir + chattr +C + SSH key + egress rules
agent-vm create
```

Note the explicit `-c qemu:///system`. libvirt's QEMU driver defaults to `qemu:///session`
for non-root users — the `libvirt` group grants socket access but doesn't change the default
URI — so bare `virsh` talks to a different hypervisor with no networks and none of your
domains. `agent-vm` pins the URI itself (`VM_LIBVIRT_URI`); only ad-hoc `virsh` needs the flag.

## Egress: what the guest can reach

Enforced **host-side** on virbr0 (`vm/nftables-agent-vm.nft`, installed by `bootstrap` to
`/etc/nftables/agent-vm-egress.nft` and loaded at boot by `agent-vm-egress.service`), so
root-in-guest can't undo it. Required, not optional: the VM runs continuously, so "keep it
stopped when not in use" no longer protects the LAN.

- **Allowed**: the public internet (Claude/OpenAI APIs, GitHub, npm, Fedora mirrors,
  `api.gitkraken.dev`, Electron downloads — no allowlist to maintain), plus DNS/DHCP to
  libvirt's dnsmasq on the host.
- **Blocked**: everything else toward the host, the LAN, RFC1918/CGNAT space, link-local.
- Host→guest (ssh, kepler's tunnel, seed pushes) is host-initiated and unaffected.

The table coexists with firewalld and libvirt (own nft table; a drop anywhere wins;
firewalld reloads only flush firewalld's table). Do **not** enable Fedora's
`nftables.service` — its config flushes the whole ruleset.

### virbr0 must be in firewalld's `libvirt` zone

Separate from our table, and the one piece of host firewall state that silently kills the
guest. The `libvirt` zone is `target: ACCEPT` and allows `dhcp`/`dns`; if `virbr0` is bound
to **no zone**, guest traffic falls to the **default** zone (`public`), which allows
neither, and every DHCPDISCOVER is dropped *before dnsmasq sees it*.

The symptom is a healthy-looking VM that cannot be reached: `virsh domstate` says running,
the guest has only an IPv6 link-local on `enp1s0`, `virsh net-dhcp-leases default` is
empty, the host's ARP entry for `VM_IP` is `INCOMPLETE`/`FAILED`, `agent-vm ssh` says **`No
route to host`**, and dnsmasq's journal shows *no DHCP lines at all* — not even a DISCOVER.
Rebooting the guest changes nothing, because the block is host-side.

libvirt binds the bridge when the network starts, but firewalld re-applies its permanent
config on every start and `--reload`, dropping a runtime-only binding — at boot that's a
race either daemon can win. So `agent-vm` asserts the binding itself, runtime **and**
`--permanent`, from `agent-vm-egress.service` (`ExecStartPost`, so every host boot) and from
`install_egress` (so `bootstrap` and `egress` do it too).

```sh
firewall-cmd --get-zone-of-interface=virbr0   # want: libvirt
agent-vm status                               # 'firewall:' line says the same
agent-vm egress                               # reasserts rules + binding
```

## Break-glass: when the guest has no network

There is no second way in, by design — so reach for `agent-vm logs` early rather than
hunting for a login:

- **The console can't help.** cloud-init is key-only (`ssh_pwauth: false`, no password on
  `dev`, `disable_root: true`), so `virsh console` reaches a login prompt with no
  credentials that exist.
- **The guest agent can't help.** It runs confined as `virt_qemu_ga_t`; through
  `virsh qemu-agent-command … guest-exec`, `systemctl`, `journalctl`, `nmcli`, `ip`, `rpm`,
  and `/var/log/journal` all return Access/Permission denied. It *can* still answer
  `virsh domifaddr --source agent`, which is how `status` sees the guest's real addresses.
  Note that a confined `nmcli` reports "NetworkManager is not running" when it merely
  can't reach it — check `busctl --system list` for `org.freedesktop.NetworkManager`
  before believing it.
- **libguestfs can.** `agent-vm logs` (`virt-log`) reads the journal off the disk image,
  read-only and safe on a live guest, though entries still buffered in the guest won't be
  there. `agent-vm rescue` (`virt-rescue`) gives a root shell on the disk to actually fix
  something — read-write, so it refuses unless the guest is stopped. Both need
  `guestfs-tools`.

Verify from the guest:

```sh
curl -sI https://api.anthropic.com   # works
nc -zvw3 192.168.122.1 22            # must FAIL (host)
nc -zvw3 192.168.1.1 22              # must FAIL (LAN)
```

## Credential model

The guest holds **no signing key and no host filesystem mounts**. Its GitHub access is a
TPM-resident SSH key registered on the account (git, read and write) and a read-only `gh`
token (API). Pushes are gated by agent permission rules, not by the key.

- **Public repos** (vscode-gitlens) are cloned anonymously by `provision.sh` into
  `~/kepler/repositories/` — kepler's own layout, so kepler tasks reuse the same checkout.
- **Private repos** (kepler itself) are **seeded from the host**: `agent-vm seed
  ~/dev/repos/kepler` pushes over host-initiated SSH into
  `~/kepler/repositories/kepler`. The clone carries the real `origin` URL (kepler
  resolves repos by remote URL); re-running `seed` updates it from the host.
- **In-guest git via the TPM key**: the guest has an emulated TPM (swtpm), and
  provisioning creates a **non-exportable** key in it — a compromised guest can use the
  key while running but never steal it. Provision prints the public half; register it on
  GitHub as an **account SSH (authentication) key** and both `git fetch` and `git push`
  work from inside, for every repo the account can reach. **Never** add it as a signing
  key — a guest-usable signing key means forged "Verified" commits — and note that the
  guest disables signing globally anyway. The swtpm state dies with the domain (`destroy`
  removes it), so a rebuild means registering a fresh key — deregister the old one.

  The key places no limit on what the guest may push, so that job belongs to the agent's
  permission layer: `permissions.ask` entries plus `autoMode.soft_deny` rules in
  `profiles/agent-vm/home/.claude/settings.json` force an explicit human confirmation for
  any push or GitHub-mutating command. Those rules are the control — keep them in place.
- **Read-only `gh` token** so agents can read issues, PRs, CI runs, and repo metadata.
  `provision.sh` **prompts** for it (early in the run, before the slow steps) when there
  is no valid token or the current one expires within 30 days; paste-and-forget, nothing
  stored on the host. Use a **fine-grained** PAT — classic tokens have no read-only mode,
  their `repo` scope is read+write — at **90 days**, restricted to the repos the guest may
  read, with every permission set to Read: Metadata, Contents, Issues, Pull requests,
  Actions, Checks, Commit statuses, Discussions, Deployments.

  Unlike the TPM key this **is** exfiltratable — a compromised guest can steal it and use
  it from anywhere — which is why it must stay read-only and repo-scoped; the 90-day
  expiry bounds the damage. It is stored by `gh auth login --with-token` in
  `~/.config/gh/hosts.yml` (plaintext; the guest is the boundary), which `gh` reads however
  it was launched, so non-interactive agents get it without `GH_TOKEN` in any environment.
  Provisioning deliberately skips `gh auth setup-git`, which would make `gh` git's HTTPS
  credential helper and turn the token into a git path — git stays on the TPM key. Revoke
  by deleting the PAT on GitHub; `gh auth logout` clears the guest's copy.
- Work can leave via the host rather than being pushed from the guest:
  `agent-vm fetch [--repo name] <branch>` → `vm/<branch>` on the host, to review and push
  there. Still the right route when you want the diff in front of you first.

Its only real credentials are the agents' own logins (Claude, codex, GitKraken), which the
agents need to reach their APIs anyway.

### Commit identity

The guest has no `~/.gitconfig` of its own. Provisioning writes a **global** one from the
host's identity, passed in by `agent-vm` — never hardcoded, since this repo is public and
the right address differs per machine (the dotfiles `work` profile resolves a work email,
`personal` a personal one). Override with `VM_GIT_NAME` / `VM_GIT_EMAIL` in
`~/.config/agent-vm.conf`.

`agent-vm` resolves that identity from an empty temp dir, not from its cwd and not with
`--global`. Both obvious spellings are wrong: plain `git config --get` reads the cwd's
repo-local config, and the dotfiles checkout has its own `user.email`, so provisioning
from there would seed a personal address into a work guest; `--global` does not expand
`[include]` and returns an **empty** email, because the address lives in
`~/.gitconfig-local` — the file the profiles swap.

Global, not per-repo, because kepler and `gh` create clones `provision.sh` never sees;
an earlier per-clone version reached only vscode-gitlens and left the rest either
identity-less (commits fail outright) or carrying whatever a tool wrote into
`.git/config`. Each provision also strips `user.*`, `commit.gpgsign`, and `tag.gpgsign`
overrides from every repo under `~/kepler/repositories/`, since local config beats global.

It is deliberately your **real, routable** address rather than a placeholder: guest
commits come back via `agent-vm fetch` and are pushed from the host, so anything else
lands on a real remote as a dead address that links to no account. Provenance comes from
the `wip/*` branch namespace `agent-vm push` uses, not from a fake email. Signing is
disabled globally (`commit.gpgsign`, `tag.gpgsign`, `tag.forceSignAnnotated`) because the
guest holds no signing key by design — tags included, since the host gitconfig sets
`tag.forceSignAnnotated`.

## Dotfiles in the guest

A **curated subset** of the host dotfiles is symlinked into the guest by provisioning:
`~/.vimrc`, `~/.vim`, `~/.gitignore`, the tmux config plus the four `tmux-*` scripts its
status line and bindings call, and the Claude config from `profiles/agent-vm/`.

The guest never runs `./install`. That repo's dotbot base is desktop-shaped (sway, i3,
waybar, weechat, plasma) and cannot be subsetted — a profile can override a base link but
never remove one — and three base links actively break this guest: `~/.bashrc` does not
source `~/.bashrc.d/*`, so mise's shims and `SSH_ASKPASS` would disappear; `~/.ssh/config`
would drop the `PKCS11Provider` block the TPM key needs, and the next provision
would append into the checkout through the symlink; `~/.claude/settings.json` would import
a host sandbox allowlist of paths that mean nothing here.

tmux-resurrect and tmux-continuum are deliberately absent — session persistence in a
disposable guest is not wanted — so `tmux.conf` guards their `run-shell` lines.

### Round trip

The guest holds a real checkout at `~/dotfiles`, **seeded from the host** rather than
cloned from the public remote: the host is the source of truth and is routinely ahead of
it, and seeding gives the return path below for free without the guest holding any
credential. It is at `~/dotfiles` and not under `~/.agent-vm` because `sync_assets` wipes
that directory on every provision.

| Direction | Command |
|---|---|
| host → guest | `agent-vm dotfiles push` (also run by `create` and `provision`) |
| guest → host | `agent-vm dotfiles pull` |
| capture hand-built guest config | `agent-vm dotfiles adopt` |

`pull` commits whatever is dirty in the guest checkout onto `vm/guest` — editing a linked
file in the guest writes straight into it, so uncommitted changes are the normal case —
then fetches that branch home as `vm/dotfiles` for review. This works because provisioning
gives the guest a global git identity with signing off; without it the in-guest commit
would fail. `push` refuses while the guest checkout is dirty, since it ends in a force
checkout.

`adopt` is the one-time capture that precedes the first link: it copies an explicit list
(`CLAUDE.md`, `settings.json`, `skills/`) out of the guest's `~/.claude` into
`profiles/agent-vm/`, never the whole directory — that also holds credentials, session
transcripts and plugin caches. Provisioning independently refuses to clobber: a real file
sitting at a link target is moved to `~/.dotfiles-preadopt/<timestamp>/`, not deleted.

Guest Claude config lives in `profiles/agent-vm/` because it encodes the guest's
credential model — the read-only `gh` token and the MCP-only path for GitHub writes are
not true of the host. There is deliberately no `install.d/profiles/agent-vm.yaml`: it
would look dotbot-installable, and installing it would drag in the base links above.

### Skills

`~/.claude/skills` is linked wholesale into the profile directory, so a skill written
inside the guest is captured by `dotfiles pull` with no extra step.

Skills meant for **both** machines live in `home/.claude/skills/` and appear in that
directory as a relative symlink — `ship-issue` is one. The host installs the same tree
per-skill through dotbot (`install.d/base/20-home.yaml`), so both sides share one copy
and an edit from either lands in the same file. Anything genuinely guest-only can just be
a real directory under `profiles/agent-vm/home/.claude/skills/` instead.

## Disks

| File | Survives `destroy`? |
| --- | --- |
| `base.qcow2` | yes (never touched after download) |
| `system.qcow2` | no — this *is* the rebuild |
| `state.qcow2` | yes, unless you confirm a second prompt |

`state.qcow2` mounts at `/mnt/state` and holds what must outlive a rebuild: the guest's
**SSH host keys** (kepler pins them with `accept-new`, which errors on a *changed* key — so
a fresh identity would break the target), **kepler-server** and its `data/app.db` session
history (also spares a tarball re-ship and the `gk` re-download), and the inspector's
`agent-sessions`. Everything else is durable while the VM lives but rebuilt from script.

Agent logins (Claude, codex) live on the system disk — a rebuild means logging in again.
Rebuilds are rare; that's the accepted cost of keeping them off the shared state disk.

## Running kepler agents in the guest

Kepler connects over SSH and ships its own `kepler-server` daemon into the guest — the
tarball vendors its own `node`, so there's nothing to install. Agents then execute in the
guest while the desktop app stays on the host. Repos and worktrees live under `~/kepler/`.

The `~/.ssh/config` entry is what makes the guest appear in kepler's target picker. Three
constraints, all from `parseSshConfig` in `src/remote/resolvers/ssh.ts`:

- It must live in **`~/.ssh/config` itself** — `Include` and `Match` are ignored.
- Only `Host`, `Hostname`, `User`, `Port`, `IdentityFile` are parsed.
- Only the **first** pattern on a `Host` line is used, so `agent-vm` is the label shown.

(After the rename from claude-vm, any old `claude-vm` target entry in kepler is stale —
pick `agent-vm` once and it re-pins.)

If you run kepler from source, build the guest's tarball first — dev mode reads
`out/remote/` directly and fails with `TarballUnavailableError` if it's empty:

```sh
cd ~/dev/repos/kepler && pnpm create:remote:linux:x64
```

A packaged kepler skips this and fetches from R2 instead. Versions are coupled, so the
tarball must match the desktop version.

Set the agent's permission mode in kepler's launcher — its mode ids pass straight through
to the CLI's `--permission-mode`; `auto` there matches `agent-vm claude`.

Three things make this survive a rebuild:

| Problem | Fix |
| --- | --- |
| DHCP lease moves on rebuild | Reserved `VM_IP` keyed to the pinned `VM_MAC` |
| Host key changes on rebuild | `/etc/ssh/ssh_host_*` persisted on the state disk |
| Rebuild wipes `~/.kepler-server` | Symlinked onto the state disk, so `data/app.db` survives |

The daemon downloads `gk` on first boot from the GitKraken release manifest
(`api.gitkraken.dev`) — public internet, so the egress rules don't interfere; it lands on
the state disk, so it's a one-time cost per state-disk lifetime.

Forwarding into the guest also needs a local SELinux module, because kepler's transport is
`ssh -L <local>:127.0.0.1:<daemon port>`. `selinux-policy` 44.6 shipped `sshd_session_t` —
the post-auth domain of the OpenSSH 9.8+ `sshd`/`sshd-session` split — as a permissive
domain; 44.7 enforces it without allowing `name_connect` to high ports, so every `ssh -L`
fails at sshd's `connect()` while plain `ssh` keeps working. The symptom is kepler
reporting `remote server at http://127.0.0.1:<port> not reachable within 15000ms` against
a daemon that is listening and answering `/api/health` from inside the guest; the AVC is in
the guest's `/var/log/audit/audit.log`. `provision.sh` builds and installs
`kepler-ssh-forward` to grant it — retire that step once Fedora ships the allows
(`sudo semodule -r kepler-ssh-forward`).

**Every forwarding mode is affected, not just `-L`.** The removal was a deliberate
hardening sweep (upstream `Remove 14 permissive domains`, 2026-06-30), so the missing
allows are the bug; each mode was reported separately and all are still open:

| Mode | Fails as | Denial | Report |
| --- | --- | --- | --- |
| `ssh -L`, ProxyJump | kepler health probe times out | `name_connect` | [RHBZ 2523732](https://bugzilla.redhat.com/show_bug.cgi?id=2523732) |
| `ssh -X` | `X11 forwarding request failed on channel 0`, then `Missing X server or $DISPLAY` | `name_bind` | [RHBZ 2524485](https://bugzilla.redhat.com/show_bug.cgi?id=2524485) |
| `ssh -R` (tcp) | `remote port forwarding failed for listen port` | `name_bind` | RHBZ 2523732 |
| `ssh -R` (unix) | `remote port forwarding failed for listen path` | `create` on `sock_file` | [#3383](https://github.com/fedora-selinux/selinux-policy/issues/3383) |
| `ssh -D` | SOCKS connects hang | `name_connect` | RHBZ 2523732 |
| `ssh -A` | `Agent forwarding disabled: couldn't create listener socket` | `create` on `sock_file` | [#3384](https://github.com/fedora-selinux/selinux-policy/issues/3384) |
| `internal-sftp` | — | — | [#3382](https://github.com/fedora-selinux/selinux-policy/issues/3382) |

The module grants the whole `port_type` attribute rather than an enumeration, because
enumerating kept coming up one port type short — `ssh -X` needs `xserver_port_t`, `ssh -R`
needs whatever the caller picked, `ssh -D` needs `http_port_t`. It also grants `sock_file`
create/unlink/getattr under `tmp_t`, `user_tmp_t`, and `ssh_home_t` for the unix-socket
forwards, which fail on file creation rather than on a port permission.

Because the permissive removal was intentional, expect upstream to grant specific
permissions rather than restore permissiveness — so when a fix lands, re-test each mode
after `semodule -r` rather than assuming all of them came back. The host picks the same
regression up whenever it takes 44.6+; it was still on 44.5 as of 2026-08-26.

## Running a guest GUI on the host

`ssh -X agent-vm` then `pnpm dev` in `~/kepler/repositories/kepler` is enough — electron
renders on the host X display (Xwayland, under GNOME) with no extra flags. `Warning: No
xauth data; using fake authentication data` is expected and harmless: GNOME's Xwayland
keeps no `~/.Xauthority`, and it admits the session user directly.

X11 forwarding tunnels every draw over ssh, so it is fine for a smoke test and sluggish for
real UI work. `waypipe` is the faster path when that bites — it is installed in the guest by
`provision.sh`, and electron needs `--ozone-platform=wayland`:

```sh
waypipe ssh agent-vm                       # then run the app from that shell
waypipe ssh agent-vm ~/bin/some-script     # or hand it ONE program
```

`waypipe ssh` parses its remote argv itself instead of handing the line to a shell, so a
compound command silently misfires — `waypipe ssh agent-vm 'cd x && ./app'` reports
`Failed to run program cd`, and `bash -c '…'` fares no better. Use an interactive shell or
a script. Electron logs `drmGetDevices2() has not found any devices` there; that is the
guest having no render node, and it software-renders past it.

## Toolchain (all per-user, all current)

- **Node via mise** (`node@lts` globally; repos with `.nvmrc`/`.tool-versions` get their
  own). Shims are on PATH for non-interactive shells too, so kepler-spawned processes
  resolve node/pnpm.
- **pnpm via corepack** shim in `~/.local/bin`; each repo's `packageManager` pins its version.
- **Claude Code** via the native installer — user-owned, self-updates in place. Provisioning
  health-checks it (`claude --version` must say "Claude Code") rather than just checking
  presence: the old npm-global root-owned install once failed mid-update and left `claude`
  resolving to a bare bun runtime that a presence check would have skipped. The install is
  pinned to the `latest` channel explicitly — a bare `install.sh` installs *stable* and pins
  self-updates there, so the repair path would quietly downgrade a guest on `latest`.
  (Anything kepler launches runs the Claude CLI pinned by its `claude-agent-sdk`, not this
  one, so `claude update` does not move it.)
- **codex** binary from GitHub releases, refreshed on every provision — but only when the
  release tag is ahead of the installed `codex --version`, so an unchanged release costs a
  redirect instead of a download.
- **`gh`** from Fedora's repos, so dnf-automatic keeps it current like everything else.
  Provisioning prompts for its read-only token when one is missing or near expiry.
- **OS**: `dnf upgrade` at provision + dnf-automatic timer with `apply_updates = yes`.

## Fixtures

`agent-vm fixtures` rebuilds the scenario repos under `~/dev/test` in the guest, generated
from script so they're always pristine:

- `stash-conflict-repro` — `greet.js` left `UU` by a conflicted stash pop, with **no**
  paused operation.
- `rebase-multi-conflict` — two automatic-rebase scenarios: three AI-resolvable steps, and
  three independent steps where the middle one is binary and escalates.

Each generator asserts its own output, including a real rebase in a throwaway worktree.

## Smoke tests

Both run during `provision` and gate the hand-maintained Electron dep list — trust a test
failing, not the list:

- **VS Code** (in the GitLens repo): downloads current stable, opens the workbench under
  Xvfb, screenshots. Superseded cached versions are pruned.
- **kepler** (when the seeded repo is present): `pnpm install`, then launches **kepler's own
  Electron** with a trivial window under Xvfb — VS Code's Electron doesn't prove kepler's.
  The script lives under `node_modules/` so the seeded worktree stays clean.

Kepler's full e2e suite is not part of provisioning — agents run it in worktrees on demand.

## Not in the guest, deliberately

Signing key · any write-capable GitHub API token · any host filesystem mount · the host's
`~/.claude/settings.json` (its sandbox allowlist is full of host paths that mean nothing
here, and the VM is already the boundary).

The SSH key *can* push — see the credential model — so pushes are held back by the agent's
permission rules rather than by the credential.

## Known gaps

- **Shared `dev` user.** Concurrent tasks share one user, so a misbehaving task can affect
  sibling worktrees (the host stays protected — that's the boundary that matters). If tasks
  start interfering, revisit per-task isolation.
- **Worktree debris accumulates.** Each kepler worktree gets its own `node_modules` (pnpm's
  store dedupes the heavy part) and any `.vscode-test` it creates (~1G per VS Code
  download). Prune when deleting worktrees; `df -h /` in the guest when in doubt.
- **No visible display.** Everything works headless (the inspector screenshots over CDP, so
  pixels are identical), but watching the UI live would need `waypipe` plus a VS Code RPM in
  the guest.
- **Don't debug rendering-timing bugs here.** A virtualized display can mask flaky
  rendering, hover/focus, and animation glitches — or invent new ones. Guest for logic, DOM,
  a11y, and screenshot-diffable layout; host for "does this feel right".
- The hand-maintained Electron dep list in `provision.sh` will drift with Fedora releases.
  The smoke tests are what catch it.
