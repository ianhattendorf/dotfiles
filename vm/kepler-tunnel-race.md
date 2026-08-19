# kepler daemon tunnel fails instantly: multiplexed `ssh -N -L` exits 0

**Status:** root-caused, reproduced standalone. Needs a fix in kepler; no clean local
workaround. Not a `claude-vm` defect — the VM, daemon, and forward all work.
**Found:** 2026-08-18. App `0.8.1-staging.202608171930.g199d2cb`; source read at
`v0.8.1-106-g8485f9b5` (different commits — verify line numbers before patching).

## Symptom

```
Error invoking remote method 'native:remote:open-target-here':
Error: ssh exited before the tunnel on local port 25801 was ready (code 0)
```

Fails in ~460 ms, every attempt, across kepler restarts.

## Root cause

`establishDaemonTunnel` builds the tunnel with `controlMasterOpts(controlPath)`
(`ssh.ts:202`) → `-o ControlMaster=auto -o ControlPath=<master socket>`, then
`-N -L 127.0.0.1:<local>:127.0.0.1:<daemon>`.

When a master is already live at that ControlPath — and it is, because kepler starts one and
runs the capability probe over it first — OpenSSH's client hands the forward to the master
and **exits 0 immediately**. The forward is owned by the master process, not by the child.

`waitForTunnelReady` (`ssh.ts:1130`) watches that child and treats *any* exit as fatal:

```js
const onExit = (code) => { childDeath ??= new Error(
  `ssh exited before the tunnel on local port ${port} was ready (code ${code})`); };
```

So a tunnel that succeeded is reported as failed, and the error path then tears the master
down — killing the working forward.

The intent to multiplex is explicit in the comment at `ssh.ts:1062` ("ride Kepler's master
when we have one"). The readiness check contradicts it.

## Evidence

Captured process sequence for one failing attempt (ppid `1075174` = kepler):

```
1327855  ssh -o ControlMaster=yes -o ControlPath=/tmp/kepler-ssh-eNzFtk/cm \
             -o ControlPersist=120 -N dev@192.168.122.50        → forks master
1327857  (backgrounded master, ppid=1)
1327858  ssh -o ControlMaster=auto -o ControlPath=…/cm  sh -c '<probe>'   lived 0.06s
1327860  ssh -o ControlMaster=auto -o ControlPath=…/cm \
             -o ExitOnForwardFailure=yes -N -L 127.0.0.1:25801:127.0.0.1:34771
                                                                          lived 0.09s  ← fails here
1327857  master exits                                                     lived 0.21s
```

Reproduced standalone, which is the decisive part — start a master, then run the tunnel
against it:

```
TUNNEL EXIT CODE = 0
LISTEN 127.0.0.1:25802          ← forward IS live
http_code=200                   ← and serving the daemon
```

The tunnel works. Only the readiness check is wrong.

## Why it looks intermittent

`ControlMaster=auto` with **no** live master promotes the tunnel child to master, so it stays
alive and the check passes. With a master already up it multiplexes and exits. Whether the
master has finished establishing when the tunnel spawns is a race, so this presents as
flaky rather than deterministic. Here it fails consistently because the probe runs over the
master first, guaranteeing it's up.

The Windows path is immune by accident: `ssh.ts:609` passes `controlPath: null`, so its
tunnel is standalone. The POSIX path at `ssh.ts:515` passes `master.controlPath`.

## Correction to the earlier reading

An earlier pass diagnosed this as a 30 s timeout race lost to the first-boot `gk` download
(`TUNNEL_READY_TIMEOUT_MS = 30_000`, `ssh.ts:139`). That was wrong. The 29 s in the first
failure was daemon install + boot + `gk` download happening *before* the tunnel step; the
tunnel then failed instantly with the same exit-0, same as every later attempt. One root
cause throughout — the download only explained the delay, never the failure.

## Fix options

1. **Don't multiplex the tunnel (smallest).** Pass `controlPath: null` at the `ssh.ts:515`
   call site so the tunnel gets `ControlMaster=no` / `ControlPath=none`. Matches what the
   Windows path already does and leaves the readiness logic untouched. Costs one extra TCP
   connection and auth per connect.
2. **Accept exit 0 when multiplexing.** Where a controlPath is in play, stop treating child
   exit as fatal and let the TCP + HTTP probes decide. Requires also retargeting teardown:
   the forward belongs to the master, so `tunnel.kill()` is meaningless and cleanup must use
   `ssh -O cancel -L …` / `-O exit`.
3. **Use the explicit mux verb.** `ssh -O forward -L …` is the designed-for-this form and
   exits 0 by contract, paired with `-O cancel`. Clearest, largest change.

Option 1 unblocks fastest; option 3 is the more honest end state.

## Still true, unrelated

`gk` is no longer vendored; the daemon downloads it on first boot
(`api.gitkraken.dev`, ~6.5 s). It lands in `~/.kepler-server/bin/` → `/mnt/state`, which
`reset` does not roll back, so it's a one-time cost per state-disk lifetime and recurs only
if `state.qcow2` is recreated. The planned nftables egress rules must keep that host
reachable or a fresh state disk can never complete a first connect.
