(in-package #:aitools.integration.structure-test)

(describe "aitools structure test (dead code)"
  (it "reports no production dead-code violations"
    (format *error-output*
            "~&U05_GATE_SPEC_START: reports no production dead-code violations~%")
    (finish-output *error-output*)
    (handler-bind
        ((type-error
           (lambda (condition)
             (declare (ignore condition))
             (sb-debug:print-backtrace :stream *error-output*))))
      (let* ((root (merge-pathnames
                    "t/integration/fixtures/reference-roots/"
                    (repository-root)))
             (records (%liveness-files root))
             (packages (%liveness-all-package-metadata records)))
        (multiple-value-bind (references local-references diagnostics)
            (%liveness-reference-index records packages)
          (declare (ignore local-references diagnostics))
          (dolist (symbol (quote ("%MAKE-LOOP-GENERATED-MODEL"
                                  "%LOOP-GENERATED-MODEL-VALUE")))
            (expect (gethash (list :production "AITOOLS.FIXTURE.ROOTS"
                                   :function symbol)
                             references)
                    :to-equal t)))
        (expect (scan-dead-code-tree (repository-root)) :to-equal nil)
        (progn
          (expect
           (%liveness-filter-allowlisted-findings
            (list "first.lisp:3: unused definition AITOOLS.FIXTURE:GENERATED-HELPER")
            (list (list :kind :unused-definition
                        :package "AITOOLS.FIXTURE"
                        :symbol "GENERATED-HELPER"
                        :reason "a macro emits the generated call")))
           :to-equal nil)
          (expect
           (%liveness-filter-allowlisted-findings
            (list "first.lisp:3: unused definition AITOOLS.FIXTURE:GENERATED-HELPER"
                  "second.lisp:47: unused definition AITOOLS.FIXTURE:GENERATED-HELPER")
            (list (list :kind :unused-definition
                        :package "AITOOLS.FIXTURE"
                        :symbol "GENERATED-HELPER"
                        :reason "a macro emits the generated call")))
           :to-equal nil))
        (let ((rejected nil))
          (handler-case
              (%liveness-filter-allowlisted-findings
               (list "first.lisp:3: unused definition AITOOLS.FIXTURE:GENERATED-HELPER")
               (list (list :kind :unused-definition
                           :package "AITOOLS.FIXTURE"
                           :symbol "STALE-HELPER"
                           :reason "this entry must correspond to a current finding")))
            (error () (setf rejected t)))
          (expect rejected :to-equal t))))
    (format *error-output*
            "~&U05_GATE_SPEC_END: reports no production dead-code violations~%")
    (finish-output *error-output*)))
