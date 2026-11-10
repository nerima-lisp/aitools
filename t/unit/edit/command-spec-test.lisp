(in-package #:aitools.edit.test)

(describe "aitools edit command specs"
  (it "expands option groups while preserving command-specific options"
    (let* ((custom-option (list :key :tx :name "custom-tx" :kind :flag))
           (spec
             (list :name "test.command"
                   :include (list :write)
                   :options (list custom-option)
                   :custom-metadata :refuse))
           (expanded (aitools.edit.application::%expand-spec spec))
           (options (getf expanded :options)))
      (expect (mapcar (lambda (option) (getf option :key)) options)
              :to-equal (list :tx :dry-run))
      (expect (getf (first options) :name) :to-equal "custom-tx")
      (expect (getf (second options) :key) :to-be :dry-run)
      (expect (getf expanded :custom-metadata) :to-be :refuse)
      (expect (length (getf spec :options)) :to-be 1))))
(describe "aitools edit write command macros"
  (it "expands write commands and registers all document dispatchers"
    (let* ((*package* (find-package "AITOOLS.EDIT.APPLICATION"))
           (write-expansion
             (macroexpand-1
              (quote
               (aitools.edit.application::define-write-command
                "doc.set"
                (ports env positionals options &key on-plan on-error)
                (declare (ignore env))
                nil))))
           (write-flow (second write-expansion))
           (registration (third write-expansion))
           (doc-expansion
             (macroexpand-1
              (quote (aitools.edit.application::define-doc-write-commands))))
           (doc-names (mapcar (function second) (rest doc-expansion)))
           (expected-names (list "doc.set" "doc.delete" "doc.merge" "doc.patch" "doc.fmt")))
      (expect (eq (first write-expansion) (find-symbol "PROGN" "COMMON-LISP")) :to-be t)
      (expect (eq (first write-flow) (find-symbol "DEFINE-FLOW" "AITOOLS.KERNEL.DOMAIN")) :to-be t)
      (expect (eq (first (second write-flow))
                  (find-symbol "PREPARE-DOC-SET/K" "AITOOLS.EDIT.APPLICATION"))
              :to-be t)
      (expect (eq (first registration) (find-symbol "SETF" "COMMON-LISP")) :to-be t)
      (expect doc-names :to-equal expected-names)
      (expect (every (function functionp)
                     (mapcar (lambda (name)
                               (gethash name aitools.edit.application::*preparers*))
                             expected-names))
              :to-be t)))
  (it "rejects invalid write command declarations"
    (let* ((*package* (find-package "AITOOLS.EDIT.APPLICATION"))
           (macro (find-symbol "DEFINE-WRITE-COMMAND" *package*))
           (normal (list (find-symbol "PORTS" *package*)
                         (find-symbol "ENV" *package*)
                         (find-symbol "POSITIONALS" *package*)
                         (find-symbol "OPTIONS" *package*)
                         (find-symbol "&KEY" "COMMON-LISP")
                         (find-symbol "ON-PLAN" *package*)
                         (find-symbol "ON-ERROR" *package*)))
           (bad-marker (copy-list normal))
           (string-marker (copy-list normal))
           (bad-plan-type (copy-list normal))
           (bad-plan-name (copy-list normal))
           (bad-error-type (copy-list normal))
           (bad-error-name (copy-list normal)))
      (setf (nth 4 bad-marker) (find-symbol "&REST" "COMMON-LISP")
            (nth 4 string-marker) "&KEY"
            (nth 5 bad-plan-type) "ON-PLAN"
            (nth 5 bad-plan-name) (find-symbol "IDENTITY" "COMMON-LISP")
            (nth 6 bad-error-type) "ON-ERROR"
            (nth 6 bad-error-name) (find-symbol "VALUES" "COMMON-LISP"))
      (flet ((rejects-p (name lambda-list)
               (handler-case
                   (progn (macroexpand-1 (list macro name lambda-list)) nil)
                 (error () t))))
        (expect (rejects-p 7 normal) :to-be t)
        (expect (rejects-p "" normal) :to-be t)
        (expect (rejects-p "doc.bad" bad-marker) :to-be t)
        (expect (rejects-p "doc.bad" string-marker) :to-be t)
        (expect (rejects-p "doc.bad" bad-plan-type) :to-be t)
        (expect (rejects-p "doc.bad" bad-plan-name) :to-be t)
        (expect (rejects-p "doc.bad" bad-error-type) :to-be t)
        (expect (rejects-p "doc.bad" bad-error-name) :to-be t)
        (expect (rejects-p "doc.bad" (subseq normal 0 3)) :to-be t))))
  (it "preserves the optional write plan precommit callback"
    (let* ((precommit-check (lambda (context) context))
           (default-plan
             (aitools.edit.application:make-write-plan :plan (function identity)))
           (checked-plan
             (aitools.edit.application:make-write-plan
              :plan (function identity) :precommit-check precommit-check)))
      (expect (null (aitools.edit.application::write-plan-precommit-check default-plan))
              :to-be t)
      (expect (eq (aitools.edit.application::write-plan-precommit-check checked-plan)
                  precommit-check)
              :to-be t)))
  (it "forwards both format plan and error continuations"
    (let* ((*package* (find-package "AITOOLS.EDIT.APPLICATION"))
           (expanded
             (macroexpand-1
              (quote
               (aitools.edit.application::%with-plan-continuations
                (aitools.kernel.domain:flow-call test-flow)))))
           (plan-continuation (third expanded))
           (error-continuation (fourth expanded)))
      (expect (eq (first expanded)
                  (find-symbol "FLOW-CALL" "AITOOLS.KERNEL.DOMAIN"))
              :to-be t)
      (expect (eq (first plan-continuation) (find-symbol "ON-PLAN" "KEYWORD"))
              :to-be t)
      (expect (member :on-plan plan-continuation) :to-be-truthy)
      (expect (eq (first error-continuation) (find-symbol "ON-ERROR" "KEYWORD"))
              :to-be t)
      (expect (equal (mapcar #'symbol-name (second error-continuation))
                     '("CODE" "MESSAGE" "&KEY" "CANDIDATES" "DIAGNOSTICS"
                       "CONFLICTS" "REPAIRS" "PATH"))
              :to-be t))))
