# Changelog

## Unreleased

- `aitools sys info` now returns `environment.unavailable` when its uptime source
  is missing or malformed; previously, its response could contain a `null`
  uptime. CPU and memory values are optional, but uptime is required for a
  complete system information response.

## v0.4.0

- Remove the top-level `delete` command. Use `rm` for files, symlinks, empty directories, and recursive trees.
- Map the shell command `rmdir` to `aitools rm`; `rmdir` is not an aitools
  command. Calling the removed top-level `delete` command returns
  `argument.invalid` with a repair suggesting `aitools rm`.
