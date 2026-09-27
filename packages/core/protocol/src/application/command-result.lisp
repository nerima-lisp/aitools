;;;; packages/core/protocol/src/application/command-result.lisp
;;;;
;;;; The command handler contract (docs/src/reference/architecture.md,
;;;; "Command handler contract"): a flow calls exactly one of three continuations
;;;; (on-ok / on-partial / on-error) instead of returning a value or writing
;;;; output itself. CALL-WITH-COMMAND-RESULT/K is the seam between that CPS
;;;; style and the ordinary Lisp value a presentation function needs to hand
;;;; up to dispatch: it builds the three continuations, calls FLOW-FUNCTION
;;;; with them, and returns whichever one fired as a COMMAND-RESULT value.
;;;; Presentation code never picks a stream or an exit code; it calls this,
;;;; gets a COMMAND-RESULT back, and hands that to dispatch (src/dispatch.lisp)
;;;; unchanged.
(in-package #:aitools.protocol.application)

(defstruct (command-result
            (:constructor %make-command-result (kind fields))
            (:copier nil))
  "KIND is :OK, :PARTIAL, or :ERROR. FIELDS is the plist of arguments the
flow passed to whichever continuation fired: for :OK/:PARTIAL, the result
plist/alist the flow built; for :ERROR, (:CODE :MESSAGE :CANDIDATES
:DIAGNOSTICS :CONFLICTS :REPAIRS)."
  (kind nil :type (member :ok :partial :error) :read-only t)
  (fields nil :type list :read-only t))

(defun call-with-command-result/k (flow-function)
  "Call FLOW-FUNCTION with :ON-OK, :ON-PARTIAL, and :ON-ERROR keyword
continuations, and return whichever one FLOW-FUNCTION calls, as a
COMMAND-RESULT. FLOW-FUNCTION must call exactly one continuation exactly
once; calling more than one, or returning without calling any, is a bug in
the flow, not a case this function recovers from."
  (block done
    (funcall flow-function
             :on-ok (lambda (fields) (return-from done (%make-command-result :ok fields)))
             :on-partial (lambda (fields) (return-from done (%make-command-result :partial fields)))
             :on-error (lambda (code message &key candidates diagnostics conflicts repairs)
                         (return-from done
                           (%make-command-result
                            :error (list :code code :message message :candidates candidates
                                        :diagnostics diagnostics :conflicts conflicts
                                        :repairs repairs)))))))
