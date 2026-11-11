(in-package #:aitools.edit.test)
(describe "Org application flow edge contracts"
  (it "checks non-string paths and each missing write capability"
    (let ((boundaries (cl-boundary-kit:make-boundary-context)))
      (expect (org-flow-error-code
               (function aitools.org.application:org-todo-set/k)
               boundaries :path nil)
              :to-equal "argument.invalid"))
    (let ((production (make-org-write-flow-boundaries)))
      (dolist (capability (list :org-open-store :org-edit :org-write))
        (expect (org-flow-error-code
                 (function aitools.org.application:org-todo-set/k)
                 (cl-boundary-kit:boundary-context-with
                  production capability nil)
                 :path "entry.org")
                :to-equal "internal.unexpected"))))
  (it "reports a source disappearing after it is classified as a file"
    (call-with-edit-workspace
     (lambda ()
       (let* ((boundaries (make-org-write-flow-boundaries))
              (original (symbol-function
                         (quote aitools.store.application:view-read-file)))
              (read-count 0))
         (put "vanishing.org" (format nil "* TODO Task~%"))
         (let ((before (octets-of "vanishing.org")))
           (cl-weave:with-mocked-functions
               (((symbol-function
                  (quote aitools.store.application:view-read-file))
                 (lambda (view path &rest arguments)
                   (incf read-count)
                   (if (string= path "vanishing.org")
                       nil
                       (apply original view path arguments)))))
             (expect (org-flow-error-code
                      (function aitools.org.application:org-todo-set/k)
                      boundaries :path "vanishing.org" :root *root*
                      :match "/TODO" :state "DONE")
                     :to-equal "input.not-found"))
           (expect (plusp read-count) :to-be t)
           (expect (octets-of "vanishing.org") :to-equalp before))))))
  (it "maps an Org writer boundary failure and preserves the source"
    (call-with-edit-workspace
     (lambda ()
       (let* ((production (make-org-write-flow-boundaries))
              (failure
                (aitools.org.domain:make-org-failure
                 :kind :internal :message "writer failed"))
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 production :org-write
                 (lambda (document)
                   (declare (ignore document))
                   (values nil failure)))))
         (put "writer.org" (format nil "#+TODO: TODO | DONE~%* TODO Task~%"))
         (let ((before (octets-of "writer.org")))
           (expect (org-flow-error-code
                    (function aitools.org.application:org-todo-set/k)
                    boundaries :path "writer.org" :root *root*
                    :match "/TODO" :state "DONE"
                    :at "2026-10-19T09:00:00" :tz "UTC")
                   :to-equal "internal.unexpected")
           (expect (octets-of "writer.org") :to-equalp before))))))
  (it "propagates agenda, clock, and outline read failures"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (expect (org-flow-error-code
                  (function aitools.org.application:org-agenda/k)
                  boundaries :path "missing.org" :root *root*
                  :from "2026-10-19" :through "2026-10-19"
                  :at "2026-10-19T09:00:00" :tz "UTC")
                 :to-equal "input.not-found")
         (expect (org-flow-error-code
                  (function aitools.org.application:org-clock-report/k)
                  boundaries :path "missing.org" :root *root* :match "/TODO")
                 :to-equal "input.not-found")
         (expect (org-flow-error-code
                  (function aitools.org.application:org-outline/k)
                  boundaries :path "missing.org" :root *root*)
                 :to-equal "input.not-found")))))
  (it "maps an outline capability failure through the application flow"
    (call-with-edit-workspace
     (lambda ()
       (let* ((production (make-org-write-flow-boundaries))
              (failure
                (aitools.org.domain:make-org-failure
                 :kind :too-large :message "outline limit exceeded"))
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 production :org-outline
                 (lambda (document limit)
                   (declare (ignore document limit))
                   (values nil failure)))))
         (put "outline.org" (format nil "* Task~%"))
         (expect (org-flow-error-code
                  (function aitools.org.application:org-outline/k)
                  boundaries :path "outline.org" :limit 1 :root *root*)
                 :to-equal "refusal.too-large")))))
  (it "rejects a missing clock-report match before reading"
    (expect (org-flow-error-code
             (function aitools.org.application:org-clock-report/k)
             (make-org-write-flow-boundaries)
             :path "report.org" :match nil)
            :to-equal "argument.invalid"))
  (it "expands the write wrapper to all result continuations"
    (let ((expanded
            (macroexpand-1
             (list (quote aitools.org.application::%org-write-flow-call)
                   (list (quote values) :ok)))))
      (labels ((contains (form symbol)
                 (or (eq form symbol)
                     (and (consp form)
                          (or (contains (car form) symbol)
                              (contains (cdr form) symbol))))))
        (expect (eq (first expanded)
                    (quote aitools.kernel.domain:flow-call))
                :to-be t)
        (expect (contains expanded :on-ok) :to-be t)
        (expect (contains expanded :on-partial) :to-be t)
        (expect (contains expanded :on-error) :to-be t)))))

