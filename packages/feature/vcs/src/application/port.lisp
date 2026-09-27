;;;; packages/feature/vcs/src/application/port.lisp
;;;;
;;;; The one side-effect boundary of the vcs context: running git. Each slot
;;;; is a function taking continuations, so a flow never sees a condition
;;;; from the process layer and a unit test injects closures over canned git
;;;; output. The production adapter is AITOOLS.VCS.INFRASTRUCTURE:
;;;; MAKE-PRODUCTION-VCS-PORTS.
(in-package #:aitools.vcs.application)

(defstruct (git-port
            (:constructor make-git-port (&key probe run status numstat at))
            (:copier nil))
  "PROBE: (funcall probe &key on-repository on-outside on-missing
on-no-directory) -- whether the working directory is inside a git work tree
(ON-REPOSITORY receives the absolute repository top and the absolute,
symlink-resolved process working directory, both without a trailing `/`),
git cannot be started, or (ON-NO-DIRECTORY receives it) the directory git
would run in does not exist.

RUN: (funcall run subcommand arguments &key octets on-success on-failure) --
run `git SUBCOMMAND ARGUMENTS...`; ON-SUCCESS receives stdout (a string, or
an octet vector when OCTETS); ON-FAILURE receives (KIND MESSAGE), KIND being
:EXIT (git ran and failed; MESSAGE is its stderr), :MISSING (git could not
be started), or :FAILED (timeout or I/O failure).

STATUS: (funcall status &key on-success on-failure) -- ON-SUCCESS receives
the plist AITOOLS.VCS.DOMAIN:STATUS-FIELDS takes.

NUMSTAT: (funcall numstat arguments &key on-success on-failure) -- ON-SUCCESS
receives one plist (:PATH :ORIGINAL-PATH :ADDED :DELETED :BINARY) per file
of `git diff --numstat ARGUMENTS...`.

AT: (funcall at directory) -- the same port running git in DIRECTORY
(absolute, or relative to the working directory)."
  (probe (error "MAKE-GIT-PORT requires :~A" (quote probe)) :type function :read-only t)
  (run (error "MAKE-GIT-PORT requires :~A" (quote run)) :type function :read-only t)
  (status (error "MAKE-GIT-PORT requires :~A" (quote status)) :type function :read-only t)
  (numstat (error "MAKE-GIT-PORT requires :~A" (quote numstat)) :type function :read-only t)
  (at (error "MAKE-GIT-PORT requires :~A" (quote at)) :type function :read-only t))

(defun port-at-root (port root)
  "PORT, or when ROOT (a directory) is given, PORT running git there."
  (if root (funcall (git-port-at port) root) port))
