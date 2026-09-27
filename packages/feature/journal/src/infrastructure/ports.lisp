;;;; packages/feature/journal/src/infrastructure/ports.lisp
(in-package #:aitools.journal.infrastructure)

(defun make-production-journal-ports (&key workspace-host open-store &allow-other-keys)
  "JOURNAL-PORTS for the composition root. WORKSPACE-HOST resolves the
workspace root; OPEN-STORE (real-root -> STORE) is the store context's
production constructor. Construction does no I/O."
  (aitools.journal.application:make-journal-ports :workspace-host workspace-host :open-store open-store))
