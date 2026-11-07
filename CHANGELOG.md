# Changelog

## v0.4.0

- Remove the top-level `delete` command. Use `rm` for files, symlinks, empty directories, and recursive trees.
- Map `rmdir` to `rm`. Calling the removed `delete` command returns `argument.invalid` with a repair suggesting `aitools rm`.
- Combine the JSON, YAML, and TOML command groups into eight commands:
  `doc get`, `doc select`, `doc diff`, `doc set`, `doc delete`,
  `doc merge`, `doc patch`, and `doc fmt`. The path extension selects the
  format; `--format` overrides it. The former groups are not aliases and
  return `argument.invalid` with the matching `doc` command as a repair.
