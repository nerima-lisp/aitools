# Changelog

## Unreleased

## v0.4.0

- Add `net get`, `net head`, and `net resolve` for HTTP requests and OS-resolved
  A and AAAA address lookup. `net resolve` does not return CNAME chains, other
  DNS record types, or DNS response codes. HTTPS requests require verified TLS;
  URLs with credentials are refused, HTTPS-to-HTTP redirects are not followed,
  and response secrets are redacted.
  Redirect policy follows the initial DNS answers: if all initial addresses are
  non-public, redirects may use public or non-public addresses; if any initial
  address is public, only public addresses are used for the initial request and
  every redirect hop. See the [net command reference](docs/src/reference/commands/net.md)
  for address-filter details.
- Add `md headings`, `md read`, and `md replace` to list, read, and replace
  Markdown sections. `md replace` requires a SHA-256 guard and preserves bytes
  outside the selected section.
- Replace the JSON, YAML, and TOML command groups with `doc get`, `doc select`,
  `doc diff`, `doc set`, `doc delete`, `doc merge`, `doc patch`, and `doc fmt`.
  For example, use `doc get` instead of `json get`, `doc set` instead of
  `yaml set`, and `doc fmt` instead of `toml fmt`. The path extension selects
  the format and `--format` overrides it. Without `--format`, a missing or
  unsupported extension returns `argument.invalid` with a format repair;
  JSON-only formatting options with YAML or TOML return the same code. Former
  group names are not aliases and suggest the corresponding `doc` command.
- Remove the built-in Git commands `git status`, `git log`, `git diff`,
  `git blame`, and `git show`; use `aitools run -- git <args>` for Git,
  `aitools run -- jj <args>` for jj, and `aitools run -- gh <args>` for GitHub
  CLI operations. Retired Git commands return `argument.invalid` with a `run`
  repair. `overview` no longer reports the Git branch, HEAD, or untracked and
  deleted counts; use `aitools run -- git status` for Git status.
- Retire the top-level `delete` command and map shell `rmdir` to `aitools rm`.
  Calling `delete` returns `argument.invalid` with a repair to `aitools rm`.
- `sys info` now includes uptime and returns `environment.unavailable` when
  its uptime source is missing or malformed. `sys metrics` now includes
  one-, five-, and fifteen-minute load averages, and returns
  `environment.unavailable` when a CPU, load-average, memory, or process source
  is missing or malformed.
- Add `sys metrics --timeout` with a 60-second default per subprocess source,
  including process listing. When its process list is truncated,
  `next_commands` includes a command that sets `--limit` to the observed count
  and preserves `--timeout`.
- Command timeouts are scoped: `run` defaults to `120s`, `wait` to `60s`, and
  `net get`, `net head`, and `net resolve` to `30s`; see the [process reference](docs/src/reference/commands/process.md)
  and [net reference](docs/src/reference/commands/net.md) for options and values.
  System probe timeouts are source-specific; see the [env reference](docs/src/reference/commands/env.md).
  `sys snapshot` defaults to a ten-minute overall deadline across all sections,
  including socket-owner scans. Each external command is limited to 60 seconds
  or the remaining budget, whichever is less; timed-out children are killed and
  reaped. Local calls are checked at their boundaries; an active bounded call
  may finish after the budget before collection stops. Directory and ancestor
  walks check between entries. `--timeout` overrides the default.
  Captured subprocess output has no memory bound.
  Detached [`bg start`](docs/src/reference/commands/process.md#bg-start) has no
  operation deadline.
- YAML and TOML values outside the shared JSON value model return
  `input.unsupported-value`.
- Security: Linux `signal` uses a pidfd to verify and signal the same process;
  Darwin rechecks the process identity before signaling.
- Security: workspace write boundaries reject paths into Git metadata,
  including resolved Git common directories and linked-worktree metadata.
- Security: `archive extract` rejects `..` entry paths and symlink escapes
  through filesystem, archive, and transaction-staged paths. It rechecks staged
  symlinks at `tx commit` and uses Unicode NFD normalization and full case
  folding for symlink components on every platform. Unsafe destinations return
  `refusal.outside-workspace` and leave the transaction open.
- Archive limits include 4,095-byte UTF-8 entry paths and link targets,
  255-byte path components, depth 2,048, 200,000 nonzero tar header records,
  65,534 ZIP central-directory entries, 16 MiB retained path metadata, 64 MiB
  archive input, and 1,024 gzip members with up to 256 MiB decompressed output
  per member. PAX and GNU metadata records count toward the tar limit, while
  all-zero end records do not. ZIP metadata counts raw names and uncompressed
  symlink targets; tar metadata counts emitted UTF-8 names and link targets.
  Exceeding a limit returns `refusal.too-large`.
- Archive entry selection, duplicate-name checks, and hard-link target lookup
  ignore empty and `.` path components while preserving case and Unicode
  spelling. On case-insensitive filesystems, aliased extraction paths return
  `refusal.exists` when applied, including at `tx commit`.

### Known limitations

- Do not rely on `net` redirect checks to recognize private IPv4 addresses
  synthesized under a network-specific RFC 6052 NAT64 prefix or encoded in a
  Teredo address: this client cannot discover network-specific NAT64 prefixes,
  and its Teredo check classifies the client IPv4 address but does not check the
  Teredo server's IPv4 address, which identifies the relay server.
- Use `signal` only when the target PID is unlikely to be reused during the
  operation: Darwin can reuse it between the recheck and `kill`, and Linux can
  miss reuse within the same clock tick before `pidfd_open`.
- Process only trusted archives, keep the extraction target out of reach of
  other processes running with the same privileges, and set `--max-bytes` below
  its 1 GiB default; another process running with the same privileges can still
  change a target after the precommit recheck, and the inflater's internal
  buffer can exceed `--max-output` before its check.
- Keep Git metadata outside bind-mounted workspace paths and avoid metadata
  symlinks into the workspace, such as a hook targeting a tracked script,
  because both alias forms remain undetected.
