# Getting started

## Build

From a checkout:

```sh
nix build                    # -> ./result/bin/aitools
./result/bin/aitools --version
```

```json
{"schema_version":1,"status":"ok","command":"version","name":"aitools","version":"0.1.1"}
```

`nix run . -- --version` runs it without creating the `result` link. The
examples below call it as `aitools`.

## List the commands

```sh
aitools schema
```

returns `{"schema_version":1,"status":"ok","command":"schema","commands":[...]}`
with a `{name, summary}` entry per command. `aitools schema <command>` returns
the command's arguments, output fields, and error codes.

## Read a file

```console
$ aitools read hello.lisp
{"schema_version":1,"status":"ok","command":"read","mode":"text","path":"hello.lisp","start_line":1,"lines":["(defun greet (name)","  (format t \"Hello, ~a\" name))"],"total_lines":2,"hash":"de2187a50599282d3a6bbf334e5a804108cf30e2005767255053ba7e94512c71","truncated":false,"encoding_errors":0,"approx_tokens":13}
```

`hash` identifies the file content; a write that selects lines by number
must pass it back as `--expect-hash`.

## Edit it, and undo the edit

```console
$ aitools edit hello.lisp --old 'Hello' --new 'Hi'
{"schema_version":1,"status":"ok","command":"edit","changes":[{"path":"hello.lisp","action":"modified","hash_before":"de2187a5...","hash_after":"5f132d76...","diff":"@@ -1,2 +1,2 @@\n (defun greet (name)\n-  (format t \"Hello, ~a\" name))\n+  (format t \"Hi, ~a\" name))\n"}],"op_id":"op-20260926T011541Z-4a125e73","strategy":"exact"}

$ aitools history
{"schema_version":1,"status":"ok","command":"history","items":[{"op_id":"op-20260926T011541Z-4a125e73","command":"aitools edit --old Hello --new Hi hello.lisp","paths":["hello.lisp"],"time":"2026-09-26T01:15:41Z"}],"total":1,"truncated":false}

$ aitools undo op-20260926T011541Z-4a125e73 --dry-run
{"schema_version":1,"status":"ok","command":"undo","changes":[{"path":"hello.lisp","action":"modified", ...}],"dry_run":true,"undoes":"op-20260926T011541Z-4a125e73"}
```

Hashes are shortened here. Without `--dry-run`, `undo` writes the change
and records it as a new operation.

## When something goes wrong

Errors come on standard error with a code, an exit code, and repairs:

```console
$ aitools edit hello.lisp --range 1 --new '(defun greet (who)'
{"schema_version":1,"status":"error","command":"edit","error":{"code":"argument.invalid","message":"this write needs --expect-hash <hash>","exit_code":1,"repairs":[{"action":"get-hash","detail":"Read the current hash, then pass it as --expect-hash.","command":"aitools info hello.lisp"}]}}
```

A shell command name gets a pointer to its replacement:

```console
$ aitools cat hello.lisp
{"schema_version":1,"status":"error","command":"cat","error":{"code":"argument.invalid","message":"unknown command cat","exit_code":1,"repairs":[{"action":"run-instead","detail":"Read a file with line numbers and range control.","command":"aitools read"}]}}
```

Next, read [Using aitools from an agent](guide/agents.md).

The outputs on this page were captured from a build of the current source
in a scratch directory; `op_id`, times, and hashes differ on every run.
