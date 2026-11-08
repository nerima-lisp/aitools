# Changelog

## Unreleased

- Security: `archive extract` refuses link targets that leave the extraction
  root, pass through an archive symlink at any target component, or pass
  through an existing destination symlink, including at the target's final
  component. Archive-symlink checks treat case and canonically equivalent
  Unicode names as the same path. Entry selection, duplicate-name checks, and
  hard-link lookup preserve case and Unicode spelling after empty and `.` path
  components are removed. Case-only file or symlink path collisions work on
  case-sensitive filesystems and are refused during the write on case-insensitive
  filesystems.
- `aitools sys info` now returns `environment.unavailable` when its uptime source
  is missing or malformed; previously, its response could contain a `null`
  uptime. CPU and memory values are optional, but uptime is required for a
  complete system information response.

## v0.4.0

- Remove the top-level `delete` command. Use `rm` for files, symlinks, empty directories, and recursive trees.
- Map the shell command `rmdir` to `aitools rm`; `rmdir` is not an aitools
  command. Calling the removed top-level `delete` command returns
  `argument.invalid` with a repair suggesting `aitools rm`.
- Combine the JSON, YAML, and TOML command groups into eight commands:
  `doc get`, `doc select`, `doc diff`, `doc set`, `doc delete`,
  `doc merge`, `doc patch`, and `doc fmt`. The path extension selects the
  format; `--format` overrides it. The former groups are not aliases and
  return `argument.invalid` with the matching `doc` command as a repair.
