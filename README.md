# aitools

[![CI](https://github.com/nerima-lisp/aitools/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/nerima-lisp/aitools/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-MkDocs%20Material-0a7a5a)](https://nerima-lisp.github.io/aitools/)

A command-line tool for AI agents that replaces the shell commands they use
for file and text work: `cat`, `grep`, `sed`, `find`, `jq`, `tar`, and
others. Every call prints one JSON object. Reads are bounded and say how to
continue; writes are guarded by the expected text, hash, or count, survive a
crash, and can be undone; known secret formats are masked in all output; and
every error names an aitools command to run next. It has no features for
people (no color, prompts, or completion). Written in Common Lisp for SBCL.

Full documentation is published at <https://nerima-lisp.github.io/aitools/>.
The source for that site lives in [docs/src/](docs/src/).

## Quick Start

```sh
aitools schema                                 # every command with a summary
aitools read src/app.lisp --range 1:40         # lines 1-40, with the file hash
aitools search 'defun build-' src              # grouped matches with context
aitools edit src/app.lisp --old 'foo' --new 'bar'
aitools history                                # journaled writes, newest first
aitools undo <op_id>                           # revert one write
```

Success goes to standard output with exit code 0, or 3 when a read was cut
at a limit. Errors go to standard error with exit code 1, or 2 when the file
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

`flake.nix` declares `x86_64-linux` and `aarch64-darwin`. CI gates
`x86_64-linux` only. The package's `bin/` also holds
`cl-process-kit-spawn`, the helper `bg start` uses to detach a process; aitools
looks for it next to its own executable, so copy both together.

## Documentation

- [Getting started](https://nerima-lisp.github.io/aitools/getting-started/)
- [Using aitools from an agent](https://nerima-lisp.github.io/aitools/guide/agents/):
  the call loop, guards, transactions, and the shell-command table
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
nix run .#test                     # the same suite, built by Nix
nix flake check                    # tests, paredit lint, Nix formatting, docs: the CI gate
nix fmt                            # format Nix sources
```

Outside `nix develop`, set `CL_SOURCE_REGISTRY` to checkouts of the
dependencies `flake.nix` pins before running `sbcl --script run-tests.lisp`.
Use `nix flake check path:.` to include files git does not track yet.

Tests live in `t/` and run under
[cl-weave](https://github.com/nerima-lisp/cl-weave). See [Development](https://nerima-lisp.github.io/aitools/project/development/)
for the layout, how to add a command, and how to regenerate the reference
pages.

## Contributing

Open a [GitHub issue](https://github.com/nerima-lisp/aitools/issues) for a
bug report or feature proposal. Changes must keep `nix flake check` green and
include tests for behavior changes.

## License

MIT. See [LICENSE](LICENSE).
