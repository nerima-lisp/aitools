(in-package #:aitools/cli)

(defun %full-command-name (invocation)
  "The invoked command's full dotted dispatch name (\"read\", \"json.get\"),
or NIL when no command was dispatched (bare `--help`/`--version`)."
  (let ((path (invocation-command-path invocation)))
    (when path
      (format nil "~{~A~^.~}" (mapcar #'command-name path)))))

(defun %display-name (command-name &key cli-p)
  "COMMAND-NAME in response form. N2 Org envelopes keep the dotted dispatch
name; repairs use CLI words."
  (and command-name
       (let ((org-command-p (and (>= (length command-name) 4)
                                 (string= "org." command-name :end2 4))))
         (if (or cli-p (not org-command-p))
             (substitute #\Space #\. command-name)
             command-name))))

(defun %generic-repair (command-name)
  (schema-repair
   (if command-name
       (format nil "aitools schema ~A" (%display-name command-name :cli-p t))
       "aitools schema")))

(defun %groups-with-subcommand (registry name)
  "The group names whose subcommand list contains a command named NAME, sorted
so the repairs read in a stable order (`bg`, `git`, `tx` for `status`)."
  (sort (loop for group being the hash-keys of (command-registry-group-commands registry)
                using (hash-value subcommands)
              when (find name subcommands :key #'command-name :test #'string=)
                collect group)
        #'string<))

(defun %group-repair-command (argv name group)
  "ARGV rebuilt as a runnable `aitools ...` line with GROUP inserted before the
command token NAME, so `--root R get /a f.json` becomes
`aitools --root R json get /a f.json`. Drops argv0 and shell-quotes every
word so an operand with spaces stays one argument."
  (let* ((tokens (rest argv))
         (pos (position name tokens :test #'string=))
         (rebuilt (if pos
                      (append (subseq tokens 0 pos) (list group) (subseq tokens pos))
                      (list* group tokens))))
    (format nil "aitools ~A" (aitools.protocol.domain:command-line rebuilt))))

(defun %group-subcommand-repairs (registry argv name)
  "The repair for a group subcommand typed without its group
(`get` for `json get`, `uuid` for `util uuid`): one `run-instead` repair per
group whose subcommand list contains a command named NAME, each the grouped
form of the invocation ARGV named, keeping its globals and operands. NIL when
NAME is no group's subcommand -- the caller then falls back to the
correspondence table."
  (loop for group in (%groups-with-subcommand registry name)
        collect (repair "run-instead"
                         (format nil "`~A` is the `~A` group's subcommand; include the group name."
                                 name group)
                         (%group-repair-command argv name group))))

(defun %unknown-name-repairs (name)
  "The correspondence-table repairs for the unknown dispatch NAME, verbatim from
the table. The repair is the canonical aitools command to use instead
(`cat` -> `aitools read`) and does not carry NAME's operands: the
correspondence data is a static hint, and the meta suite pins each repair to
the table's exact command."
  (repairs-for-unknown-name name))

(defun %command-group (registry command)
  "The group name whose subcommand list contains COMMAND by identity, or NIL."
  (loop for group being the hash-keys of (command-registry-group-commands registry)
          using (hash-value subcommands)
        when (member command subcommands :test #'eq)
          return group))

(defun %usage-error-command-name (registry command)
  "The dotted dispatch name of the command in scope for a usage error. A
group subcommand keeps its group (`bg.logs`, not `logs`) so the envelope and
its `aitools schema bg logs` repair name the command the agent actually
invoked. NIL when no command was in scope."
  (when command
    (let ((group (%command-group registry command)))
      (if group
          (format nil "~A.~A" group (command-name command))
          (command-name command)))))

(defun %unknown-command-repairs (registry argv name)
  "The repairs for an unresolved dispatch NAME (docs/src/reference/errors.md,
Unknown command names). A foreign name in the correspondence table gets the table's canonical command
verbatim (`fmt` -> `aitools transform`, `uuid` -> `aitools util uuid`), without
operands, even when a group also has a subcommand of that name; the meta suite
pins each to the table. A bare group subcommand NOT in the table (`get`)
points at its grouped form and carries the globals and operands so the repair
runs as given."
  (cond ((null name) (list (%generic-repair nil)))
        ((aitools.protocol.domain:correspondence-name-p name) (%unknown-name-repairs name))
        ((%group-subcommand-repairs registry argv name))
        (t (%unknown-name-repairs name))))
