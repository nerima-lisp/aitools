;;;; data/domain/env/tools-data.lisp
;;;;
;;;; The default list `sys tools` probes when no name is
;;;; given. Each entry is (NAME . VERSION-ARGUMENTS); most tools answer
;;;; `--version`, `go` only answers the `version` subcommand.
(in-package #:aitools.data)

(defparameter *env-default-tools*
  '(("git" "--version")
    ("nix" "--version")
    ("sbcl" "--version")
    ("node" "--version")
    ("npm" "--version")
    ("cargo" "--version")
    ("rustc" "--version")
    ("go" "version")
    ("python3" "--version")
    ("make" "--version")
    ("gcc" "--version")
    ("clang" "--version")
    ("docker" "--version")
    ("rg" "--version")
    ("jq" "--version"))
  "Default `sys tools` probes, in output order.")

(defparameter *env-version-arguments-default* '("--version")
  "Version arguments for a tool named on the command line that is not in
*ENV-DEFAULT-TOOLS*.")

(export '(*env-default-tools* *env-version-arguments-default*))
