# Changelog

## Unreleased

- `aitools sys info` now returns `environment.unavailable` when its uptime source
  is missing or malformed. Unlike optional CPU and memory values, uptime is
  required to produce a complete system information response.
