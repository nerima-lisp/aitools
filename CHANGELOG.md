# Changelog

## Unreleased

- `aitools sys info` now returns `environment.unavailable` when its uptime source
  is missing or malformed; previously, its response could contain a `null`
  uptime. CPU and memory values are optional, but uptime is required for a
  complete system information response.

## v0.4.0

- Security: Plan-time archive checks reject targets that traverse archive symlinks before their final component (for example, `x/d -> ..` followed by `s -> x/d/..`), existing destination symlinks, and symlink parents. Case-folded NFD path keys catch aliases and duplicate entry names.
- Remove the top-level `delete` command. Use `rm` for files, symlinks, empty directories, and recursive trees.
- Map the shell command `rmdir` to `aitools rm`; `rmdir` is not an aitools
  command. Calling the removed top-level `delete` command returns
  `argument.invalid` with a repair suggesting `aitools rm`.
- Combine the JSON, YAML, and TOML command groups into eight commands:
  `doc get`, `doc select`, `doc diff`, `doc set`, `doc delete`,
  `doc merge`, `doc patch`, and `doc fmt`. The path extension selects the
  format; `--format` overrides it. The former groups are not aliases and
  return `argument.invalid` with the matching `doc` command as a repair.
