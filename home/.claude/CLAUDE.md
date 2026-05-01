# Global Instructions

## Git & Sandbox

Git operations that access the network (fetch, push, pull, clone, ls-remote) or perform commit signing (commit, rebase, merge, cherry-pick, tag) require resources the sandbox blocks. When any `git` command fails with signature-related errors (e.g. `incorrect passphrase supplied to decrypt private key`, `failed to write commit object`), network errors (e.g. `Could not read from remote repository`, `Connection refused`, `Forbidden`), or filesystem errors (e.g. `Permission denied`, `Operation not permitted`, `EACCES`), immediately retry with `dangerouslyDisableSandbox: true` — do not ask first.
