# Changelog

## Unreleased

- Security: `archive extract` refuses link targets that leave the extraction
  root, pass through an archive symlink at any target component, or cross an
  existing destination symlink, including at the target's final component.
  For archive-symlink checks, planning compares path components after Unicode
  NFD normalization and full case folding.
- Entry selection, duplicate-name checks, and hard-link target lookup compare
  names after removing empty and `.` path components while preserving case and
  Unicode spelling. Case-sensitive filesystems keep case-only paths distinct.
  On a case-insensitive destination filesystem, `archive extract` returns
  `refusal.exists` during commit if the filesystem aliases those paths.
- `aitools sys info` returns `environment.unavailable` when its uptime source
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
