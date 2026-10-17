# aitools

[![CI](https://github.com/nerima-lisp/aitools/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/nerima-lisp/aitools/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-MkDocs%20Material-0a7a5a)](https://nerima-lisp.github.io/aitools/)

A command-line tool for AI agents with built-in file and text operations and
an external-program runner. Use the built-ins instead of `cat`, `grep`, `sed`,
`find`, `jq`, or `tar`, or invoke installed programs through `aitools run`.
Every call prints one JSON object. File reads support bounded windows and
continuation; journaled writes are guarded by the expected text, hash, or count,
support crash recovery and guarded undo; known secret formats are masked in JSON output; and
every error names an aitools command to run next. It has no features for
people (no color, prompts, or completion). Written in Common Lisp for SBCL.

## Quick Start

```sh
# After `nix build` in the Install section below:
export PATH="$PWD/result/bin:$PATH"
aitools schema                                 # every command with a summary
aitools read src/app.lisp --range 1:40         # lines 1-40, with the file hash
aitools search 'defun build-' src              # grouped matches with context
aitools run -- awk 'END { print NR }' README.md # an external program, JSON result
aitools sys metrics --limit 10                # CPU, memory, and busiest processes
```

For guarded edits and undo, follow the
[write workflow](docs/src/guide/agents.md) using text that exists in the target
file. `history` lists journaled writes; pass a returned operation ID to `undo`.

`run` passes arguments directly to the program. It does not interpret shell
syntax. For a pipeline, invoke a shell explicitly:

```sh
aitools run -- sh -c 'grep "aitools" README.md | sort -u'
```

External programs keep their normal filesystem and network access. Their
writes are not guarded or journaled by aitools and cannot be reverted with
`aitools undo`. `--root` is not a sandbox. Inspect `exit_code`, `signal`, and
`timed_out` in the result: a child failure or timeout does not make `run`
fail. It returns exit code 0, or 3 when `--grep` results exceed `--grep-limit`.
Other output trimming still returns 0. `--stdout-to` writes unmodified stdout
to a new file without secret masking.

Success goes to standard output with exit code 0, or 3 for a partial result.
Errors go to standard error with exit code 1, or 2 when the file
was not in the state the call assumed:

```console
$ aitools cat README.md
{"schema_version":1,"status":"error","command":"cat","error":{"code":"argument.invalid","message":"unknown command cat","exit_code":1,"repairs":[{"action":"run-instead","detail":"Read a file with line numbers and range control.","command":"aitools read"}]}}
```

## Install

From a checkout:

```sh
nix build              # -> ./result/bin/aitools
./result/bin/aitools --version
```

`flake.nix` declares `x86_64-linux` and `aarch64-darwin`. CI requires both
platforms: the Linux Nix test suite and the Darwin delivery checks plus the
full host test suite. See [development](https://nerima-lisp.github.io/aitools/project/development/)
for the commands. The package's `bin/` also holds
`cl-process-kit-spawn`, the helper `bg start` uses to detach a process; aitools
looks for it next to its own executable, so copy both together.

## Documentation

- [Getting started](https://nerima-lisp.github.io/aitools/getting-started/)
- [Using aitools from an agent](https://nerima-lisp.github.io/aitools/guide/agents/):
  the call loop, guards, transactions, and the shell-command table
- [State inspection](https://nerima-lisp.github.io/aitools/guide/state-inspection/):
  host and workspace observations, with coverage limits
- [Migrating session commands](https://nerima-lisp.github.io/aitools/guide/session-migration/):
  native candidates and external execution boundaries
- [Commands](https://nerima-lisp.github.io/aitools/reference/commands/)
- [JSON output](https://nerima-lisp.github.io/aitools/reference/json-schema/)
- [Errors and exit codes](https://nerima-lisp.github.io/aitools/reference/errors/)
- [Transactions](https://nerima-lisp.github.io/aitools/reference/transactions/)
- [Architecture](https://nerima-lisp.github.io/aitools/reference/architecture/)
- [Benchmarks](https://nerima-lisp.github.io/aitools/reference/benchmarks/)

The documentation is the specification of aitools's behavior.

## For AI agents

[skills/aitools/SKILL.md](skills/aitools/SKILL.md) is an agent skill that
tells an AI agent how to use aitools. A contract test checks it against the
real CLI. Load it into an agent that supports skills, or give it to the agent
as instructions.

## Development

```sh
nix develop                        # SBCL with the dependencies on CL_SOURCE_REGISTRY
sbcl --script run-tests.lisp       # the test suite
nix run .#test                     # full suite on the host against Nix-built delivery; no skips/todos
nix flake check                    # syntax, formatting, docs; Linux also runs the full suite
nix fmt                            # format Nix sources
```

Outside `nix develop`, set `CL_SOURCE_REGISTRY` to checkouts of the
dependencies `flake.nix` pins before running `sbcl --script run-tests.lisp`.
Use `nix flake check path:.` to include files git does not track yet.
On Darwin, both `nix flake check` and `nix run .#test` are required.

Tests live in `t/` and run under
[cl-weave](https://github.com/nerima-lisp/cl-weave). See [Development](https://nerima-lisp.github.io/aitools/project/development/)
for the layout, how to add a command, and how to regenerate the reference
pages.

## Contributing

Open a [GitHub issue](https://github.com/nerima-lisp/aitools/issues) for a
bug report or feature proposal. Changes must pass the platform's required
[development gates](docs/src/project/development.md) and include tests for
behavior changes.

## License

MIT. See [LICENSE](LICENSE).
