# Changelog

## v0.4.0

- Remove the top-level `delete` command. Use `rm` for files, symlinks, empty directories, and recursive trees.
- Map `rmdir` to `rm`. Calling the removed `delete` command returns `argument.invalid` with a repair suggesting `aitools rm`.
