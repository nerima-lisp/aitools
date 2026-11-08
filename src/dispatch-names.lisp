(in-package #:aitools/cli)

(defun %full-command-name (invocation)
  "The invoked command's full dotted dispatch name (\"read\", \"doc.get\"),
or NIL when no command was dispatched (bare `--help`/`--version`)."
  (let ((path (invocation-command-path invocation)))
    (when path
      (format nil "~{~A~^.~}" (mapcar #'command-name path)))))

(defun %display-name (command-name)
  "Return the command name used in envelopes, with spaces between group names and subcommands."
  (and command-name
       (substitute #\Space #\. command-name)))

(defun %generic-repair (command-name)
  (schema-repair
   (if command-name
       (format nil "aitools schema ~A" (%display-name command-name))
       "aitools schema")))

(defun %groups-with-subcommand (registry name)
  "The group names whose subcommand list contains a command named NAME, sorted
so the repairs read in a stable order (`bg`, `tx` for `status`)."
  (sort (loop for group being the hash-keys of (command-registry-group-commands registry)
                using (hash-value subcommands)
              when (find name subcommands :key #'command-name :test #'string=)
                collect group)
        #'string<))

(defun %group-repair-command (argv name group)
  "ARGV is rebuilt as a runnable `aitools ...` line by inserting each GROUP
path token before NAME. For example, `--root R set /a` becomes
`aitools --root R org todo set /a`. Drops argv0 and shell-quotes every word
so operands containing spaces remain one argument."
  (let* ((tokens (rest argv))
         (group-tokens (uiop:split-string group :separator "."))
         (pos (position name tokens :test #'string=))
         (rebuilt (if pos
                      (append (subseq tokens 0 pos) group-tokens (subseq tokens pos))
                      (append group-tokens tokens))))
    (format nil "aitools ~A" (aitools.protocol.domain:command-line rebuilt))))

(defun %group-subcommand-repairs (registry argv name)
  "The repair for a group subcommand typed without its group
(`get` for `doc get`, `uuid` for `util uuid`): one `run-instead` repair per
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
  "Return retired structured-document repairs before generic foreign-name suggestions."
  (let* ((tokens (rest argv))
         (group (and (member name (list "json" "yaml" "toml")
                              :test (function string=))
                     name))
         (index (and group (position group tokens :test (function string=))))
         (verb (and index (nth (1+ index) tokens)))
         (retired (and group verb (format nil "~A.~A" group verb))))
    (cond ((and retired
                (aitools.protocol.domain:correspondence-name-p retired))
           (%unknown-name-repairs retired))
          ((null name) (list (%generic-repair nil)))
          ((aitools.protocol.domain:correspondence-name-p name)
           (%unknown-name-repairs name))
          ((%group-subcommand-repairs registry argv name))
          (t (%unknown-name-repairs name)))))
