# aitools

aitools is a command-line tool for AI agents that replaces the shell commands
they use for file and text work (`cat`, `grep`, `sed`, `find`, `jq`, `tar`,
and others). People are not its audience: it has no color, prompts, or shell
completion.

- **One JSON object per call.** Success goes to standard output, errors to
  standard error, and the exit code separates a precondition failure (2) and
  a cut-off read (3) from other errors (1).
- **Bounded reads.** Every read has a limit. A cut result says so and
  carries the command that reads the next part.
- **Guarded writes.** A write names the text it replaces or the hash and
  count it expects, and writes nothing when the file differs.
- **Crash-safe writes with undo.** Every write goes through one protocol that
  finishes or discards an interrupted operation on the next run, and is
  journaled so `aitools undo <op_id>` can revert it.
- **Transactions.** Writes from several calls can be staged, inspected, and
  committed as one operation.
- **Secret masking.** Known secret formats are replaced with
  `[REDACTED_SECRET]` in every output.
- **Repairs.** Every error includes at least one aitools command to run next.
  Calling a shell command name such as `cat` returns the aitools command
  that replaces it.

aitools is written in Common Lisp for SBCL and built with Nix.

## Where to go next

- [Getting started](getting-started.md): build the binary and run the first
  commands.
- [Using aitools from an agent](guide/agents.md): the call loop, guards,
  transactions, and the shell-command correspondence table.
- [Commands](reference/commands.md): every command's arguments and output.
- [Architecture](reference/architecture.md): contexts, layers, ports, and the
  CPS audit.

These pages are the specification of aitools's behavior.
[Commands](reference/commands.md#not-provided) lists the options and
commands that are not provided.
