# Changelog

## Unreleased

- `aitools sys info` now returns `environment.unavailable` when its uptime source
  is missing or malformed; previously, its response could contain a `null`
  uptime. CPU and memory values are optional, but uptime is required for a
  complete system information response.

## v0.4.0

- Security: `archive extract` rejects targets that leave the extraction root or traverse an archive symlink before their final component. With `x/d -> ..`, a target of `x/d/x/d/../../outside` is refused. It also rejects target paths that cross existing symlinks under the extraction root and entries whose parents cross archive or destination symlinks. `--to` is resolved before planning: an in-workspace symlink target is used as the extraction root, while a target outside the workspace is refused. Case-folded NFD path keys make case and canonical-equivalent names compare equal for symlink and duplicate checks.
- Remove the top-level `delete` command. Use `rm` for files, symlinks, empty directories, and recursive trees.
- Map the shell command `rmdir` to `aitools rm`; `rmdir` is not an aitools
  command. Calling the removed top-level `delete` command returns
  `argument.invalid` with a repair suggesting `aitools rm`.
- Combine the JSON, YAML, and TOML command groups into eight commands:
  `doc get`, `doc select`, `doc diff`, `doc set`, `doc delete`,
  `doc merge`, `doc patch`, and `doc fmt`. The path extension selects the
  format; `--format` overrides it. The former groups are not aliases and
  return `argument.invalid` with the matching `doc` command as a repair.
