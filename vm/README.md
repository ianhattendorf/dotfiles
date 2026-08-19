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
| `egress` | Reapply the host-side nftables rules after editing them |
| `ssh [cmd…]` | Shell in (or run a one-off command) |
| `start` / `stop` / `status` | Lifecycle; `status` includes egress state |
| `destroy` | Delete domain + system disk; state disk only with a second confirmation |

First run: `bootstrap` → `create` → log into the agents (`agent-vm claude`,
`agent-vm codex`). Update flows: `seed` again to refresh the kepler repo from the host;
`provision` again to refresh everything else.

## One-time host setup

```sh
sudo dnf install -y @virtualization
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

Verify from the guest:

```sh
curl -sI https://api.anthropic.com   # works
nc -zvw3 192.168.122.1 22            # must FAIL (host)
nc -zvw3 192.168.1.1 22              # must FAIL (LAN)
```

## Credential model

The guest holds **no GitHub credentials** — no signing key, no push-capable SSH key, no
`gh` token, no host filesystem mounts.

- **Public repos** (vscode-gitlens) are cloned anonymously by `provision.sh` into
  `~/kepler/repositories/` — kepler's own layout, so kepler tasks reuse the same checkout.
- **Private repos** (kepler itself) are **seeded from the host**: `agent-vm seed
  ~/dev/repos/kepler` pushes over host-initiated SSH into
  `~/kepler/repositories/kepler`. The clone carries the real `origin` URL (kepler
  resolves repos by remote URL); re-running `seed` updates it from the host.
- **In-guest fetch via TPM deploy key**: the guest has an emulated TPM (swtpm), and
  provisioning creates a **non-exportable** key in it — a compromised guest can use the
  key while running but never steal it. Provision prints the public half; add it on
  GitHub as a **read-only, per-repo deploy key** and `git fetch origin` works from
  inside for those repos. Keep it read-only (pushes stay host-side) and **never**
  register it as an account signing key — a guest-usable signing key means forged
  "Verified" commits. The swtpm state dies with the domain (`destroy` removes it), so a
  rebuild means registering a fresh key — deregister the old one.
- Work leaves via the host: `agent-vm fetch [--repo name] <branch>` → `vm/<branch>` on the
  host. Review and push from the host, which has the credentials.

Its only real credentials are the agents' own logins (Claude, codex, GitKraken), which the
agents need to reach their APIs anyway.

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

## Toolchain (all per-user, all current)

- **Node via mise** (`node@lts` globally; repos with `.nvmrc`/`.tool-versions` get their
  own). Shims are on PATH for non-interactive shells too, so kepler-spawned processes
  resolve node/pnpm.
- **pnpm via corepack** shim in `~/.local/bin`; each repo's `packageManager` pins its version.
- **Claude Code** via the native installer — user-owned, self-updates in place. Provisioning
  health-checks it (`claude --version` must say "Claude Code") rather than just checking
  presence: the old npm-global root-owned install once failed mid-update and left `claude`
  resolving to a bare bun runtime that a presence check would have skipped.
- **codex** binary from GitHub releases, refreshed to latest on every provision.
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

Signing key · push-capable SSH keys · `gh` token · any host filesystem mount · the host's
`~/.claude/settings.json` (its sandbox allowlist is full of host paths that mean nothing
here, and the VM is already the boundary).

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
