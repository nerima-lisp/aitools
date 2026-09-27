;;;; packages/feature/inspect/src/infrastructure/ports.lisp
;;;;
;;;; The production INSPECT-PORTS. Every effect inspect needs is another
;;;; context's adapter, built by the composition root and handed in here
;;;; (docs/src/reference/architecture.md, "Ports and the composition root");
;;;; this layer only assembles them.
(in-package #:aitools.inspect.infrastructure)

(defun make-production-inspect-ports (&key state-directory-function workspace-host open-store text-source
                                      &allow-other-keys)
  "INSPECT-PORTS over the composition root's workspace host, text source,
store opener, and state-directory function. Performs no I/O."
  (make-inspect-ports :workspace-host workspace-host
                      :text-source text-source
                      :open-store open-store
                      :state-directory-function state-directory-function))
