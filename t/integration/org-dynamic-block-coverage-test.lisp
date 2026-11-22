(in-package #:aitools.edit.test)

(defparameter +dyn-clock+
  "CLOCK: [2026-10-01 Thu 10:00]--[2026-10-01 Thu 11:00] =>  1:00")

;; A CLOCK line without times: org-kit ignores it and signals org-clock-warning.
(defparameter +dyn-bad-clock+ "CLOCK: [2025-01-01 Wed]--[2025-01-02 Thu]")

(defparameter +dyn-time-arguments+ '("--at" "2026-10-15T12:00:00" "--tz" "UTC"))

(defun %dyn-ok (arguments command)
  (multiple-value-bind (code envelope stream)
      (aitools.test.support:run-aitools *root* arguments)
    (expect code :to-be 0)
    (expect stream :to-be :stdout)
    (expect (aitools.test.support:envelope-value envelope "command")
            :to-equal command)
    (expect (aitools.test.support:envelope-value envelope "status")
            :to-equal "ok")
    envelope))

(defun %dyn-error (arguments command error-code)
  (multiple-value-bind (code envelope stream)
      (aitools.test.support:run-aitools *root* arguments)
    (let ((exit-code (aitools.protocol.domain:error-code-exit-code error-code)))
      (expect code :to-be exit-code)
      (expect stream :to-be :stderr)
      (expect (aitools.test.support:envelope-value envelope "command")
              :to-equal command)
      (expect (aitools.test.support:envelope-value envelope "status")
              :to-equal "error")
      (expect (aitools.test.support:envelope-value envelope "error" "code")
              :to-equal error-code))
    envelope))

(defun %dyn-update-arguments (path block &rest options)
  (append (list "org" "dynamic-block" "update" path "--block" (princ-to-string block))
          +dyn-time-arguments+ options))

(defun %dyn-warnings (envelope)
  "The (path line source) of each warning in ENVELOPE, after checking the fixed fields."
  (let ((warnings (aitools.test.support:envelope-value envelope "warnings")))
    (expect (vectorp warnings) :to-be t)
    (loop for index below (length warnings)
          for warning = (aref warnings index)
          do (expect (aitools.test.support:envelope-value warning "code")
                     :to-equal "org.clock-ignored")
             (expect (aitools.test.support:envelope-value warning "message")
                     :to-equal "A CLOCK line that could not be parsed was left out of the totals.")
          collect (list (aitools.test.support:envelope-value warning "path")
                        (aitools.test.support:envelope-value warning "line")
                        (aitools.test.support:envelope-value warning "source")))))

(defun %dyn-warning-entries (vector)
  "The (path line source) of each warning object in VECTOR."
  (loop for warning across vector
        collect (list (json-field warning "path")
                      (json-field warning "line")
                      (json-field warning "source"))))

(defun %dyn-engine (source &optional source-name)
  (multiple-value-bind (document failure)
      (aitools.org.infrastructure:parse-org-octets
       (sb-ext:string-to-octets source :external-format :utf-8)
       :source-name source-name)
    (expect failure :to-be nil)
    (aitools.org.domain:org-document-engine-value document)))

(defun %dyn-disk-view ()
  (aitools.store.application::disk-view (open-store *root*)))

(defun %dyn-agenda-report (&optional (arguments ":scope agenda"))
  (put "report.org"
       (format nil "#+BEGIN: clocktable ~A~%old~%#+END:~%" arguments)))

(defmacro %with-dyn-kit-condition (condition &body body)
  `(%with-org-adapter-condition org-kit:update-dynamic-block ,condition ,@body))

(describe "Org dynamic block update through the dispatcher"
  (it "updates only the selected block and leaves the file untouched on dry-run"
    (call-with-edit-workspace
     (lambda ()
       (put "blocks.org"
            (format nil "* Task~%CLOCK: [2026-10-01 Thu 10:00]--[2026-10-01 Thu 11:30] =>  1:30~%#+BEGIN: clocktable :scope file~%old one~%#+END:~%#+BEGIN: clocktable :scope file :block 2026-10-01~%old two~%#+END:~%"))
       (let* ((before (octets-of "blocks.org"))
              (dry (%dyn-ok (%dyn-update-arguments "blocks.org" 2 "--dry-run")
                            "org dynamic-block update")))
         (expect (aitools.test.support:envelope-value dry "changed") :to-be t)
         (expect (aitools.test.support:envelope-value dry "block") :to-be 2)
         (expect (%dyn-warnings dry) :to-equal nil)
         (expect (octets-of "blocks.org") :to-equalp before)
         (let ((written (%dyn-ok (%dyn-update-arguments "blocks.org" 2)
                                 "org dynamic-block update")))
           (expect (aitools.test.support:envelope-value written "block") :to-be 2)
           (expect (%dyn-warnings written) :to-equal nil))
         (expect (search "old one" (text "blocks.org")) :to-be-truthy)
         (expect (search "old two" (text "blocks.org")) :to-be nil)
         (expect (search "1:30" (text "blocks.org")) :to-be-truthy)))))
  (it "refuses a block number that selects no dynamic block"
    (call-with-edit-workspace
     (lambda ()
       (put "blocks.org"
            (format nil "* Task~%#+BEGIN: clocktable :scope file~%old~%#+END:~%"))
       (let ((before (octets-of "blocks.org")))
         (dolist (block '(2 5))
           (let ((envelope (%dyn-error (%dyn-update-arguments "blocks.org" block)
                                       "org dynamic-block update" "argument.invalid")))
             (expect (aitools.test.support:envelope-value envelope "error" "message")
                     :to-equal "--block does not select a dynamic block")))
         (expect (octets-of "blocks.org") :to-equalp before)))))
  (it "passes cl-org-kit's specific message through for argument errors"
    (call-with-edit-workspace
     (lambda ()
       (put "task/a.org" (format nil "* Work~%~A~%" +dyn-clock+))
       (dolist (case-row
                (list
                 (list :id :unsupported-parameter
                       :source (format nil "#+BEGIN: clocktable :bogus 1~%old~%#+END:~%")
                       :contains '("Unsupported clocktable parameter" "bogus"))
                 (list :id :archive-scope
                       :source (format nil "#+BEGIN: clocktable :scope agenda-with-archives~%old~%#+END:~%")
                       :contains '("scope"))
                 (list :id :partial-range
                       :source (format nil "#+BEGIN: clocktable :tstart \"2026-10-01\"~%old~%#+END:~%")
                       :contains '("tstart"))
                 (list :id :agenda-without-glob
                       :source (format nil "#+BEGIN: clocktable :scope agenda~%old~%#+END:~%")
                       :contains '("--agenda-glob is required" ":scope agenda"))
                 (list :id :agenda-glob-matches-nothing
                       :source (format nil "#+BEGIN: clocktable :scope agenda~%old~%#+END:~%")
                       :options '("--agenda-glob" "missing/*.org")
                       :contains '("--agenda-glob matched no Org files" ":scope agenda"))
                 (list :id :unregistered-name
                       :source (format nil "#+BEGIN: no-such-handler~%old~%#+END:~%")
                       :contains '("no-such-handler"))))
         (put "report.org" (getf case-row :source))
         (let* ((before (octets-of "report.org"))
                (envelope (%dyn-error
                           (apply #'%dyn-update-arguments "report.org" 1
                                  (getf case-row :options))
                           "org dynamic-block update" "argument.invalid"))
                (message (aitools.test.support:envelope-value envelope "error" "message")))
           (expect (stringp message) :to-be t)
           (dolist (fragment (getf case-row :contains))
             (expect (list (getf case-row :id) fragment (and (search fragment message) t))
                     :to-equal (list (getf case-row :id) fragment t)))
           (expect (octets-of "report.org") :to-equalp before)))))))

(describe "Org dynamic block adapter failures"
  (it "maps a missing block, an invalid time, and engine failures"
    (let* ((engine (%dyn-engine (format nil "#+BEGIN: clocktable :scope file~%old~%#+END:~%")))
           (options (list :block 1 :at "2026-10-15T12:00:00" :tz "UTC"
                          :dst-policy "compatible")))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::edit-org-dynamic-block
           (%dyn-engine "* No block") options)
        (expect (list updated fields) :to-equal '(nil nil))
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
        (expect (aitools.org.domain:org-failure-message failure)
                :to-equal "--block does not select a dynamic block"))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::edit-org-dynamic-block
           engine (list :block 1 :at "not-a-time" :tz "UTC" :dst-policy "compatible"))
        (expect (list updated fields) :to-equal '(nil nil))
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))
      (dolist (case-row
               (list
                (list :id :clocktable-parameter :kind :argument
                      :condition (make-condition 'org-kit:org-clocktable-parameter-error
                                                 :message "Unsupported clocktable parameter"
                                                 :reason :unsupported-parameter
                                                 :parameter "bogus" :value "1"))
                (list :id :dynamic-block :kind :argument
                      :condition (make-condition 'org-kit:org-dynamic-block-error
                                                 :message "No handler is registered for this dynamic block"
                                                 :reason :unregistered-name :name "demo"))
                (list :id :resource-limit :kind :too-large
                      :condition (make-condition 'org-kit:org-resource-limit-error
                                                 :message "limit" :limit-name :max-output-length
                                                 :limit 1 :actual 2))
                (list :id :generic :kind :internal
                      :condition (make-condition 'org-kit:org-kit-error :message "failed"))))
        (%with-dyn-kit-condition (getf case-row :condition)
          (multiple-value-bind (updated fields failure)
              (aitools.org.infrastructure::edit-org-dynamic-block engine options)
            (let ((condition (getf case-row :condition)))
              (expect (list (getf case-row :id) updated fields)
                      :to-equal (list (getf case-row :id) nil nil))
              (expect (list (getf case-row :id)
                            (aitools.org.domain:org-failure-kind failure)
                            (aitools.org.domain:org-failure-message failure))
                      :to-equal (list (getf case-row :id) (getf case-row :kind)
                                      (princ-to-string condition)))
              (expect (and (search (org-kit:org-kit-error-message condition)
                                   (aitools.org.domain:org-failure-message failure))
                           t)
                      :to-be t)))))))
  (it "names --agenda-glob when the agenda selects no documents"
    (let ((engine (%dyn-engine (format nil "#+BEGIN: clocktable :scope agenda~%old~%#+END:~%"))))
      (dolist (case-row '((nil "--agenda-glob is required for :scope agenda")
                          (("*.org") "--agenda-glob matched no Org files for :scope agenda")))
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::edit-org-dynamic-block
             engine (list :block 1 :at "2026-10-15T12:00:00" :tz "UTC" :dst-policy "compatible"
                          :agenda-glob (first case-row)
                          :agenda-loader (lambda () (values nil nil))))
          (expect (list updated fields) :to-equal '(nil nil))
          (expect (list (aitools.org.domain:org-failure-kind failure)
                        (aitools.org.domain:org-failure-message failure))
                  :to-equal (list :argument (second case-row)))))))
  (it "uses the agenda loader failure over the update outcome"
    (let ((engine (%dyn-engine (format nil "#+BEGIN: clocktable :scope agenda~%old~%#+END:~%")))
          (failure (aitools.org.domain:make-org-failure :kind :too-large :message "agenda too large")))
      (multiple-value-bind (updated fields actual)
          (aitools.org.infrastructure::edit-org-dynamic-block
           engine (list :block 1 :at "2026-10-15T12:00:00" :tz "UTC" :dst-policy "compatible"
                        :agenda-loader (lambda () (values nil failure))))
        (expect (list updated fields) :to-equal '(nil nil))
        (expect actual :to-be failure))))
  (it "attributes a warning to every document holding that text on that line"
    (let* ((first (%dyn-engine (format nil "* A~%~A~%" +dyn-bad-clock+) "one.org"))
           (second (%dyn-engine (format nil "* A~%~A~%" +dyn-bad-clock+) "two.org"))
           (shifted (%dyn-engine (format nil "* A~%~%~A~%" +dyn-bad-clock+) "shifted.org"))
           (valid (%dyn-engine (format nil "* A~%~A~%" +dyn-clock+) "valid.org"))
           (heading (%dyn-engine (format nil "* A~%* ~A~%" +dyn-bad-clock+) "heading.org"))
           (warning (make-condition 'org-kit:org-clock-warning :line 2 :source +dyn-bad-clock+))
           (other (make-condition 'org-kit:org-clock-warning :line 2 :source "CLOCK: nothing")))
      (expect (%dyn-warning-entries
               (aitools.org.infrastructure::%org-clock-warning-entries
                (list warning warning other)
                (list first shifted valid heading second)))
              :to-equal (list (list "one.org" 2 +dyn-bad-clock+)
                              (list "two.org" 2 +dyn-bad-clock+)))
      (expect (length (aitools.org.infrastructure::%org-clock-warning-entries nil (list first)))
              :to-be 0)
      (expect (aitools.org.infrastructure::%org-document-clock-at-p first 2 +dyn-bad-clock+)
              :to-be t)
      (expect (aitools.org.infrastructure::%org-document-clock-at-p first 3 +dyn-bad-clock+)
              :to-be nil))))
