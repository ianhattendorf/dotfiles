#!/usr/bin/env bash
# Runs INSIDE the guest, over SSH, as the unprivileged VM user. Idempotent — safe to
# re-run against a live guest to pick up changes or repair a broken install.
#
# The guest is a continuously-updated dev box, not a frozen image: the OS auto-updates,
# Claude Code self-updates in place, and re-running this script refreshes everything else.
# Keep it credential-free: public clones only — private repos arrive via `agent-vm seed`.
set -euo pipefail

GITLENS_REMOTE=${GITLENS_REMOTE:-https://github.com/gitkraken/vscode-gitlens.git}
# kepler's own layout: it reuses clones it finds here, so provisioning, seeding, and
# kepler tasks all share one checkout per repo.
REPOS_DIR=$HOME/kepler/repositories
GITLENS_DIR=$REPOS_DIR/vscode-gitlens
KEPLER_DIR=$REPOS_DIR/kepler # seeded from the host — never cloned or pulled from here
STATE_MNT=${STATE_MNT:-/mnt/state}
STATE_DEV=${STATE_DEV:-/dev/vdb}

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$1"; }

# Everything user-level lands in ~/.local/bin (first on the guest's PATH) or mise shims.
# Shims rather than `mise activate` alone so kepler-spawned NON-interactive processes
# resolve node/pnpm too. This export covers the rest of this script; ~/.bashrc.d/mise.sh
# below covers every later shell.
export PATH="$HOME/.local/share/mise/shims:$HOME/.local/bin:$PATH"
# Provision runs on a TTY (`ssh -t`), where corepack would stop to confirm its first
# pnpm download; agents hit the same prompt if they ever run on one.
export COREPACK_ENABLE_DOWNLOAD_PROMPT=0
mkdir -p "$HOME/.local/bin"

# --- packages -----------------------------------------------------------------
# Electron's runtime deps. This list is hand-maintained and will drift with Fedora
# releases — the smoke tests at the bottom are what actually catch that, so don't
# trust the list, trust a test failing.
step 'Installing packages'
sudo dnf install -y -q \
	gtk3 nss alsa-lib at-spi2-atk at-spi2-core cups-libs libdrm mesa-libgbm \
	libxkbcommon libxkbcommon-x11 libXcomposite libXdamage libXfixes libXrandr \
	libXtst libXScrnSaver pango cairo cairo-gobject gdk-pixbuf2 libsecret \
	xorg-x11-server-Xvfb \
	git tmux jq ripgrep tar xz gcc-c++ make python3 \
	tpm2-pkcs11 tpm2-pkcs11-tools tpm2-tools

# Resolve by file path, not package name — xdpyinfo has moved between packages across
# Fedora releases and dnf will find whatever currently provides it.
sudo dnf install -y -q /usr/bin/xdpyinfo

step 'Upgrading OS packages'
sudo dnf upgrade -y -q

step 'Enabling automatic updates'
# dnf5 renamed both the plugin and the timer; take whichever this release ships so a
# Fedora bump doesn't silently disable updates.
if sudo dnf install -y -q dnf5-plugin-automatic 2>/dev/null; then
	auto_timer=dnf5-automatic.timer
else
	sudo dnf install -y -q dnf-automatic
	auto_timer=dnf-automatic.timer
fi
[[ -f /etc/dnf/automatic.conf ]] \
	&& sudo sed -i 's/^apply_updates.*/apply_updates = yes/' /etc/dnf/automatic.conf
# Covers both a missing config file and a shipped one with no apply_updates line.
grep -q '^apply_updates = yes' /etc/dnf/automatic.conf 2>/dev/null \
	|| printf '[commands]\napply_updates = yes\n' | sudo tee -a /etc/dnf/automatic.conf >/dev/null
sudo systemctl enable --now "$auto_timer"

# --- mise / node ----------------------------------------------------------------
step 'Installing mise + Node (lts)'
[[ -x $HOME/.local/bin/mise ]] || curl -fsSL https://mise.run | sh

mkdir -p "$HOME/.bashrc.d" # Fedora's stock ~/.bashrc sources ~/.bashrc.d/*
cat > "$HOME/.bashrc.d/mise.sh" <<'EOF'
# Shims first so non-interactive processes (kepler agents, ssh commands) resolve
# node/pnpm; full activation only where there's a terminal to hook.
export PATH="$HOME/.local/share/mise/shims:$HOME/.local/bin:$PATH"
# corepack's first-download confirmation would wedge anything holding a TTY.
export COREPACK_ENABLE_DOWNLOAD_PROMPT=0
case $- in *i*) eval "$("$HOME/.local/bin/mise" activate bash)" ;; esac
EOF

# lts satisfies every current tenant (gitlens needs >=22.18, kepler needs >=24), and mise
# honors a repo's .nvmrc/.tool-versions when one asks for something else.
mise use -g node@lts

# pnpm via corepack: the shim in ~/.local/bin survives node version switches, and each
# repo's packageManager field pins its own pnpm version.
mise exec -- corepack enable --install-directory "$HOME/.local/bin"

# --- Claude Code ----------------------------------------------------------------
step 'Installing Claude Code'
# Native installer: self-contained, user-owned, self-updates in place. Never npm-global —
# a root-owned tree plus a user-run auto-updater is exactly how the previous VM broke
# (update failed with no_permissions and left `claude` resolving to a bare bun runtime).
# Health check rather than presence check: that breakage still had `claude` on PATH.
claude_healthy() { claude --version 2>/dev/null | grep -q 'Claude Code'; }
claude_healthy || curl -fsSL https://claude.ai/install.sh | bash
claude_healthy || { echo 'error: claude failed its health check after install' >&2; exit 1; }

# --- codex ----------------------------------------------------------------------
step 'Installing codex'
# No self-updater, so refresh to the latest release on every provision. The binary inside
# the tarball is named after the asset stem.
codex_tmp=$(mktemp -d)
curl -fsSL https://github.com/openai/codex/releases/latest/download/codex-x86_64-unknown-linux-musl.tar.gz \
	| tar -xz -C "$codex_tmp"
install -m 0755 "$codex_tmp/codex-x86_64-unknown-linux-musl" "$HOME/.local/bin/codex"
rm -rf "$codex_tmp"
codex --version >/dev/null || { echo 'error: codex failed to run after install' >&2; exit 1; }

# --- TPM-resident git deploy key ----------------------------------------------
# The guest has an emulated TPM (swtpm on the host). A key created in it is
# NON-EXPORTABLE: a compromised guest can use it while running but never steal it.
# The public half is printed at the end — register it on GitHub as a READ-ONLY
# per-repo deploy key so the guest can `git fetch` private repos itself. Pushes
# stay host-side (`agent-vm fetch`), and it must never be an account signing key.
step 'TPM-resident git deploy key'
PKCS11_LIB=/usr/lib64/pkcs11/libtpm2_pkcs11.so
TPM_PUBKEY=''
if [[ -e /dev/tpmrm0 ]]; then
	sudo usermod -aG tss "$(id -un)"

	# The PIN gates nothing meaningful (in-guest use is conceded by the threat model —
	# the TPM's contribution is non-exportability), so a locally stored PIN is fine.
	pin_file=$HOME/.config/agent-vm-tpm-pin
	if [[ ! -f $pin_file ]]; then
		mkdir -p "$HOME/.config"
		(umask 077 && head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n' > "$pin_file")
	fi
	pin=$(<"$pin_file")

	# usermod doesn't apply to this already-running session, so reach the TPM via sg.
	# The pkcs11 store lives at ~/.tpm2_pkcs11.
	if [[ ! -f $HOME/.tpm2_pkcs11/tpm2_pkcs11.sqlite3 ]]; then
		sg tss -c 'tpm2_ptool init' >/dev/null
		sg tss -c "tpm2_ptool addtoken --pid=1 --label=agent-vm --sopin='$pin' --userpin='$pin'" >/dev/null
		sg tss -c "tpm2_ptool addkey --label=agent-vm --userpin='$pin' --algorithm=ecc256 --key-label=git-deploy" >/dev/null
	fi

	# ssh asks for the PKCS#11 PIN interactively; agents have no TTY, so route the
	# prompt through SSH_ASKPASS (force: OpenSSH >=8.4 uses it even without DISPLAY).
	cat > "$HOME/.local/bin/tpm-pin-askpass" <<EOF
#!/bin/sh
cat '$pin_file'
EOF
	chmod 0700 "$HOME/.local/bin/tpm-pin-askpass"
	cat > "$HOME/.bashrc.d/tpm-ssh.sh" <<EOF
export SSH_ASKPASS="\$HOME/.local/bin/tpm-pin-askpass"
export SSH_ASKPASS_REQUIRE=force
EOF

	mkdir -p "$HOME/.ssh" && chmod 700 "$HOME/.ssh"
	if ! grep -q 'PKCS11Provider' "$HOME/.ssh/config" 2>/dev/null; then
		cat >> "$HOME/.ssh/config" <<EOF
# TPM-resident deploy key — read-only fetch of registered private repos.
Host github.com
	User git
	PKCS11Provider $PKCS11_LIB
EOF
		chmod 600 "$HOME/.ssh/config"
	fi

	TPM_PUBKEY=$(sg tss -c "ssh-keygen -D $PKCS11_LIB" 2>/dev/null | head -1) || true
	[[ -n $TPM_PUBKEY ]] || echo 'warning: TPM key exists but public key could not be read' >&2
else
	echo 'note: no TPM device — recreate the VM with VM_TPM=1 to enable the in-guest deploy key' >&2
fi

# --- persistent state disk ------------------------------------------------------
# Survives destroy/recreate: the guest's SSH host keys (kepler's pinned target) and
# kepler-server's session database live here.
step 'Preparing persistent state disk'
if [[ -b $STATE_DEV ]]; then
	# blkid MUST run as root. Unprivileged it can't read the device and still exits 0 with no
	# output, which reads as "filesystem present" — that silently skipped mkfs and left this
	# disk unformatted while everything downstream looked like it had worked.
	if [[ -z $(sudo blkid -o value -s TYPE "$STATE_DEV" 2>/dev/null) ]]; then
		echo "formatting $STATE_DEV"
		sudo mkfs.ext4 -q -L agent-state "$STATE_DEV"
	fi

	sudo mkdir -p "$STATE_MNT"
	# Match by device, not label: pre-rename disks carry the old claude-state label and
	# relabeling a mounted filesystem mid-provision isn't worth the churn.
	grep -q "^$STATE_DEV $STATE_MNT " /etc/fstab \
		|| echo "$STATE_DEV $STATE_MNT ext4 defaults,nofail 0 2" | sudo tee -a /etc/fstab >/dev/null

	# `nofail` is wanted (a missing state disk must not block boot) but it also makes mount(8)
	# exit 0 when it mounted nothing, so its status proves nothing. Assert the mountpoint.
	mountpoint -q "$STATE_MNT" || sudo mount "$STATE_MNT" || true
	mountpoint -q "$STATE_MNT" \
		|| { echo "error: $STATE_DEV exists but did not mount at $STATE_MNT" >&2; exit 1; }

	sudo mkdir -p "$STATE_MNT/agent-sessions"
	sudo chown -R "$(id -un):$(id -gn)" "$STATE_MNT"
	echo "mounted $(findmnt -no SOURCE,SIZE "$STATE_MNT")"
else
	echo "warning: $STATE_DEV absent — host keys and kepler sessions will not survive a rebuild" >&2
fi

# --- persistent SSH identity ------------------------------------------------------
# Anything that pinned this guest's host key breaks after a rebuild unless the identity is
# stable. kepler connects with StrictHostKeyChecking=accept-new, which auto-pins a new host
# but ERRORS on a changed key, so a regenerated identity means a broken remote target.
step 'Persisting SSH host keys'
if mountpoint -q "$STATE_MNT"; then
	if [[ -f $STATE_MNT/ssh-host-keys/ssh_host_ed25519_key ]]; then
		sudo cp -a "$STATE_MNT"/ssh-host-keys/ssh_host_* /etc/ssh/
		# Restarting only replaces the listener; already-established sessions (including this
		# one) are forked children and survive.
		sudo systemctl restart sshd
		echo 'restored host keys from state disk'
	else
		sudo mkdir -p "$STATE_MNT/ssh-host-keys"
		sudo cp -a /etc/ssh/ssh_host_* "$STATE_MNT/ssh-host-keys/"
		echo 'saved host keys to state disk'
	fi
else
	echo 'warning: no state disk — host keys will change on rebuild, breaking pinned clients' >&2
fi

# --- kepler-server ----------------------------------------------------------------
# kepler ships its own daemon here over SSH, vendoring its own node — nothing to install.
# But data/app.db is its single source of truth for agent sessions, so keep the whole tree
# on the persistent disk and a rebuild won't wipe agent history or force a tarball re-ship.
step 'Linking kepler-server to persistent storage'
if mountpoint -q "$STATE_MNT"; then
	mkdir -p "$STATE_MNT/kepler-server"
	if [[ -d $HOME/.kepler-server && ! -L $HOME/.kepler-server ]]; then
		cp -a "$HOME/.kepler-server/." "$STATE_MNT/kepler-server/" 2>/dev/null || true
		rm -rf "$HOME/.kepler-server"
	fi
	ln -sfn "$STATE_MNT/kepler-server" "$HOME/.kepler-server"
	echo "linked ~/.kepler-server -> $STATE_MNT/kepler-server"
else
	echo 'warning: no state disk — kepler agent history will not survive a rebuild' >&2
fi

# --- GitLens repo -------------------------------------------------------------------
step 'Cloning GitLens'
mkdir -p "$REPOS_DIR"
if [[ ! -d $GITLENS_DIR/.git ]]; then
	git clone -q "$GITLENS_REMOTE" "$GITLENS_DIR"
else
	# Continuously updating, but --ff-only: an agent's local commits are never munged.
	git -C "$GITLENS_DIR" pull -q --ff-only \
		|| echo 'warning: gitlens pull skipped (diverged or dirty)' >&2
fi

cd "$GITLENS_DIR"
git config commit.gpgsign false # no signing key in the guest, by design
git config user.name 'Agent VM'
git config user.email 'agent-vm@localhost'

# Point the inspector's named-session dir at the persistent disk so `sign_in` survives
# a rebuild. .vscode-test is already gitignored.
if [[ -d $STATE_MNT/agent-sessions ]]; then
	mkdir -p .vscode-test
	[[ -L .vscode-test/agent-sessions ]] || rm -rf .vscode-test/agent-sessions
	ln -sfn "$STATE_MNT/agent-sessions" .vscode-test/agent-sessions
fi

step 'Installing GitLens dependencies'
pnpm install --frozen-lockfile

# --- guest Claude config --------------------------------------------------------------
# Deliberately NOT the host's settings.json: that carries a sandbox allowlist full of
# host paths that mean nothing here. The VM is the boundary.
step 'Writing guest Claude config'
mkdir -p "$HOME/.claude"
if [[ ! -f $HOME/.claude/settings.json ]]; then
	cat > "$HOME/.claude/settings.json" <<'EOF'
{
	"includeCoAuthoredBy": false
}
EOF
fi

# --- smoke tests ------------------------------------------------------------------
# Both launch a real Electron under Xvfb through Playwright. They are the gate on the
# hand-maintained package list above: VS Code covers the Electron the GitLens inspector
# runs, the kepler test covers the (different) Electron kepler ships.

run_under_xvfb() {
	# xvfb-run ships with Fedora's Xvfb package, but fall back to driving Xvfb directly
	# rather than failing the whole provision if that ever stops being true.
	if command -v xvfb-run >/dev/null; then
		xvfb-run -a -s '-screen 0 1920x1080x24' "$@"
	else
		Xvfb :99 -screen 0 1920x1080x24 >/dev/null 2>&1 &
		local xvfb_pid=$!
		# shellcheck disable=SC2064
		trap "kill $xvfb_pid 2>/dev/null || true" RETURN
		DISPLAY=:99 "$@"
	fi
}

step 'Smoke testing VS Code Electron under Xvfb'
# Must live inside the repo: bare specifiers resolve relative to the importing FILE, not
# the cwd, so a script in /tmp cannot see the repo's node_modules. .vscode-test is ignored.
mkdir -p "$GITLENS_DIR/.vscode-test"
smoke=$GITLENS_DIR/.vscode-test/smoke.mjs
cat > "$smoke" <<'EOF'
const { _electron } = await import('@playwright/test');
const { downloadAndUnzipVSCode } = await import('@vscode/test-electron/out/download.js');

const exe = await downloadAndUnzipVSCode('stable');
const app = await _electron.launch({
	executablePath: exe,
	args: [
		'--no-sandbox',
		'--disable-gpu-sandbox',
		'--disable-updates',
		'--skip-welcome',
		'--skip-release-notes',
		'--disable-workspace-trust',
		'--user-data-dir=/tmp/smoke-user-data',
		'/tmp',
	],
});
const win = await app.firstWindow();
await win.waitForSelector('.monaco-workbench', { timeout: 60_000 });
await win.screenshot({ path: '/tmp/smoke.png' });
// close() can wedge even after Electron is gone — leftover transport/downloader handles
// keep the event loop alive, and the process idles forever. The screenshot is the real
// assertion, so bound the close and exit explicitly.
await Promise.race([app.close(), new Promise((r) => setTimeout(r, 15_000))]);
console.log('smoke: workbench rendered');
process.exit(0);
EOF
(cd "$GITLENS_DIR" && run_under_xvfb node "$smoke")
[[ -s /tmp/smoke.png ]] || { echo 'smoke: no screenshot produced' >&2; exit 1; }

# downloadAndUnzipVSCode('stable') resolves the CURRENT stable each run, so the cache
# otherwise accumulates one ~1G dir per release. Keep only the one just used.
ls -dt "$GITLENS_DIR"/.vscode-test/vscode-linux-x64-* 2>/dev/null | tail -n +2 | xargs -r rm -rf

if [[ -d $KEPLER_DIR/.git ]]; then
	step 'Installing kepler dependencies'
	cd "$KEPLER_DIR"
	pnpm install --frozen-lockfile

	step 'Smoke testing kepler Electron under Xvfb'
	# Lives under node_modules/ (gitignored) so the seeded worktree stays clean —
	# `agent-vm seed` pushes refuse a dirty checked-out branch.
	smoke_dir=$KEPLER_DIR/node_modules/.agent-vm-smoke
	mkdir -p "$smoke_dir"
	cat > "$smoke_dir/main.cjs" <<'EOF'
const { app, BrowserWindow } = require('electron');
app.whenReady().then(() => {
	const win = new BrowserWindow({ show: true, width: 800, height: 600 });
	win.loadURL('data:text/html,<h1>agent-vm smoke</h1>');
});
EOF
	cat > "$smoke_dir/smoke.mjs" <<'EOF'
const { _electron } = await import('@playwright/test');

const app = await _electron.launch({
	args: ['--no-sandbox', new URL('./main.cjs', import.meta.url).pathname],
});
const win = await app.firstWindow();
await win.waitForSelector('h1', { timeout: 60_000 });
await win.screenshot({ path: '/tmp/kepler-smoke.png' });
// Same bounded close as the VS Code smoke: the screenshot is the assertion.
await Promise.race([app.close(), new Promise((r) => setTimeout(r, 15_000))]);
console.log('kepler smoke: window rendered');
process.exit(0);
EOF
	run_under_xvfb node "$smoke_dir/smoke.mjs"
	[[ -s /tmp/kepler-smoke.png ]] || { echo 'kepler smoke: no screenshot produced' >&2; exit 1; }
else
	echo "note: kepler repo not present — seed it (agent-vm seed ~/dev/repos/kepler) and re-run provision" >&2
fi

step 'Provisioning complete'
echo "repos:  $REPOS_DIR"
echo "state:  $STATE_MNT (survives rebuild)"
if [[ -n $TPM_PUBKEY ]]; then
	echo
	echo 'TPM deploy key — add as a READ-ONLY deploy key on each private repo the guest'
	echo 'may fetch (github repo -> Settings -> Deploy keys). Never as a signing key.'
	echo
	echo "  $TPM_PUBKEY"
fi
echo
echo 'Next:'
echo '  agent-vm claude    # log into Claude'
echo '  agent-vm codex     # log into codex'
echo '  nc -zvw3 192.168.122.1 22   # from the guest: must FAIL (egress rules)'
