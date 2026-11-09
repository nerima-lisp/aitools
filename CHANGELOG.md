# Changelog

## Unreleased

- Security: `archive extract` refuses symlink targets that leave the resolved
  extraction root, pass through an archive or transaction-staged symlink at
  any target component, or cross an existing destination symlink, including
  the target's final component. During planning, it checks filesystem path
  components without following symlinks, plus symlinks already staged in the
  transaction and symlinks in the current archive. Staged and archive symlink
  components use Unicode NFD normalization and full case folding on every
  platform. With `--tx`, `tx commit` rechecks every staged symlink against the
  workspace root before writing. An unsafe link returns
  `refusal.outside-workspace` and leaves the transaction open to edit or abort.
- Archive extraction limits entry paths to 4,095 UTF-8 bytes, path components
  to 255 UTF-8 bytes, path depth to 2,048 nonempty, non-`.` components, and
  link targets to 4,095 UTF-8 bytes. It accepts at most 200,000 nonzero tar
  header records; all-zero end-of-archive records do not count, while PAX and
  GNU metadata headers do. ZIP archives may contain at most 65,534
  central-directory entries. Archive readers also cap the combined entry-name
  and link-target metadata byte count at 16 MiB per archive. ZIP counts raw name
  lengths and uncompressed symlink target sizes; tar counts emitted UTF-8 name
  and link-target bytes. This limits the path and target text retained across
  entries. Larger inputs return `refusal.too-large`.
- Archive entry paths containing `..` are refused. Entry selection,
  duplicate-name checks, and hard-link target lookup compare names after
  removing empty and `.` path components while preserving case and Unicode
  spelling. Case-sensitive filesystems keep case-only paths distinct.
  On a case-insensitive destination filesystem, applying the extraction returns
  `refusal.exists` if the filesystem aliases the paths. With `--tx`, this
  occurs when `tx commit` applies the staged changes.
- `aitools sys info` returns `environment.unavailable` when its uptime source
  is missing or malformed; previously, its response could contain a `null`
  uptime. CPU and memory values are optional, but uptime is required for a
  complete system information response.

## v0.4.0

- Use `rm` for files, symlinks, empty directories, and recursive trees.
- Map the shell command `rmdir` to `aitools rm`. Calling the removed top-level
  `delete` command returns
  `argument.invalid` with a repair suggesting `aitools rm`.
- Combine the JSON, YAML, and TOML command groups into eight commands:
  `doc get`, `doc select`, `doc diff`, `doc set`, `doc delete`,
  `doc merge`, `doc patch`, and `doc fmt`. The path extension selects the
  format; `--format` overrides it. The former groups are not aliases and
  return `argument.invalid` with the matching `doc` command as a repair.
