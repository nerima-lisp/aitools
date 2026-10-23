(in-package #:aitools.integration.cps-contract-test)

(defparameter *legacy-cps-on-key-allowlist*
  '(:on-key-defun
    ("packages/core/kernel/src/domain/json-model.lisp" "JSON-MODEL-RESOLVE/K")
    ("packages/core/kernel/src/domain/unified-diff-patch.lisp" "APPLY-HUNKS/K")
    ("packages/feature/edit/src/application/pipeline-store.lisp" "%DRY-RUN-IN-TX/K")
    ("packages/feature/edit/src/application/pipeline-store.lisp" "RUN-WRITE-COMMAND/K")))
