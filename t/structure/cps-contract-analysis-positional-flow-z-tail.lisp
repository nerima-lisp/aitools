(in-package #:aitools.integration.cps-contract-test)

(defun %positional-continuation-invocations (body variable function-symbol)
  (loop for (form visible) in (%terminal-call-forms body (list (cons variable variable)) function-symbol)
        for invocation = (and (consp form)
                              (some
                               (lambda (entry)
                                 (and (consp entry) (eq (cdr entry) variable)))
                               visible)
                              (or (%operator-name-p form "FUNCALL") (%operator-name-p form "APPLY"))
                              (%callback-invocation form variable visible))
        when invocation
          collect invocation))
