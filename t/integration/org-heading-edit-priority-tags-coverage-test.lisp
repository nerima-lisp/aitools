(in-package #:aitools.edit.test)
(defparameter *heading-edit-source*
  (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))

(defun %heading-edit-engine (&optional (source *heading-edit-source*))
  (aitools.org.domain:org-document-engine-value (%org-coverage-document source)))

(defun %heading-edit-options (&rest extra)
  (append (list :match "/TODO" :at "2026-10-19T09:00:00" :tz "UTC"
                :dst-policy "compatible")
          extra))

(defun %heading-edit-text (engine)
  "Serialize ENGINE with the production Org writer."
  (multiple-value-bind (octets failure)
      (aitools.org.infrastructure::write-org-octets engine)
    (expect failure :to-be nil)
    (sb-ext:octets-to-string octets :external-format :utf-8)))

(defun %heading-edit-headline (engine)
  "The first headline of ENGINE as seen by the Org parser, re-reading its text."
  (let* ((reparsed (%heading-edit-engine (%heading-edit-text engine))))
    (find :headline (org-kit:node-children (org-kit:document-root reparsed))
          :key #'org-kit:node-type)))

(defun %heading-edit-failure-kind (values-list)
  "The failure kind of a (UPDATED FIELDS FAILURE) triple that must be a failure."
  (destructuring-bind (updated fields failure) values-list
    (expect (null updated) :to-be t)
    (expect fields :to-be nil)
    (aitools.org.domain:org-failure-kind failure)))

(defmacro %with-heading-edit-error (function condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function (quote ,function))
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))

(defun %heading-edit-limited-engine (&optional (source *heading-edit-source*))
  "An engine parsed under an input limit equal to SOURCE's length, so any edit that grows it exceeds the limit."
  (let ((aitools.org.infrastructure::*org-kit-limits*
          (org-kit:make-limits :max-input-length (length source))))
    (%heading-edit-engine source)))

(defun %heading-edit-flow-fields (flow-arguments)
  (apply #'org-flow-ok #'aitools.org.application:org-heading-edit/k
         (make-org-write-flow-boundaries)
         :path "edit.org" (append flow-arguments (org-write-time-arguments))))

(defun %heading-edit-flow-error (flow-arguments)
  (apply #'org-flow-error-code #'aitools.org.application:org-heading-edit/k
         (make-org-write-flow-boundaries)
         :path "edit.org" (append flow-arguments (org-write-time-arguments))))

(describe "Org priority edit adapter"
  (it "sets, replaces, and deletes a priority"
    (let ((engine (%heading-edit-engine (format nil "#+TODO: TODO | DONE~%* TODO Task~%"))))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-priority-edit
           engine (%heading-edit-options :priority "A") nil)
        (expect failure :to-be nil)
        (expect fields :to-be nil)
        (expect (%heading-edit-text updated)
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#A] Task~%"))))
    (let ((engine (%heading-edit-engine)))
      (expect (%heading-edit-text
               (nth-value 0 (aitools.org.infrastructure::%org-priority-edit
                             engine (%heading-edit-options :priority "C") nil)))
              :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#C] Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-priority-edit
           engine (%heading-edit-options :priority "C") t)
        (expect failure :to-be nil)
        (expect fields :to-be nil)
        (expect (%heading-edit-text updated)
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO Task [1/2] :a:b:~%- [ ] x~%- [X] y~%")))))
  (it "returns selection failures before editing"
    (let ((engine (%heading-edit-engine)))
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-priority-edit
                 engine (list :match "/ABSENT" :priority "A" :at "2026-10-19T09:00:00"
                              :tz "UTC" :dst-policy "compatible")
                 nil)))
              :to-be :selection-no-match)))
  (it "maps edit, resource, and generic Org kit errors to failures"
    (let ((engine (%heading-edit-engine)))
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-priority-edit
                 engine (%heading-edit-options :priority "Z") nil)))
              :to-be :argument)
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-priority-edit
                 (%heading-edit-limited-engine
                  (format nil "#+TODO: TODO | DONE~%* TODO Task~%"))
                 (%heading-edit-options :priority "A") nil)))
              :to-be :too-large)
      (%with-heading-edit-error
          org-kit:set-heading-priority
          (make-condition 'org-kit:org-kit-error :message "priority failed")
        (expect (%heading-edit-failure-kind
                 (multiple-value-list
                  (aitools.org.infrastructure::%org-priority-edit
                   engine (%heading-edit-options :priority "A") nil)))
                :to-be :internal)))))

(describe "Org tags edit adapter"
  (it "adds without duplicating, removes, and replaces tags in order"
    (let ((engine (%heading-edit-engine)))
      (flet ((tags (operation tags)
               (multiple-value-bind (updated fields failure)
                   (aitools.org.infrastructure::%org-tags-edit
                    engine (%heading-edit-options :tags tags) operation)
                 (expect failure :to-be nil)
                 (expect fields :to-be nil)
                 (org-kit:node-property (%heading-edit-headline updated) :tags))))
        (expect (tags :tags-add "a,c") :to-equal '("a" "b" "c"))
        (expect (tags :tags-remove "a") :to-equal '("b"))
        (expect (tags :tags-replace "z,y") :to-equal '("z" "y")))
      (expect (%heading-edit-text
               (nth-value 0 (aitools.org.infrastructure::%org-tags-edit
                             engine (%heading-edit-options :tags "c") :tags-add)))
              :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :a:b:c:~%- [ ] x~%- [X] y~%"))))
  (it "treats an empty tag list as no tags"
    (let ((engine (%heading-edit-engine)))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-tags-edit
           engine (%heading-edit-options :tags "") :tags-replace)
        (expect failure :to-be nil)
        (expect fields :to-be nil)
        (expect (org-kit:node-property (%heading-edit-headline updated) :tags)
                :to-be nil))
      (expect (org-kit:node-property
               (%heading-edit-headline
                (nth-value 0 (aitools.org.infrastructure::%org-tags-edit
                              engine (%heading-edit-options :tags "") :tags-add)))
               :tags)
              :to-equal '("a" "b"))))
  (it "returns selection failures before editing"
    (expect (%heading-edit-failure-kind
             (multiple-value-list
              (aitools.org.infrastructure::%org-tags-edit
               (%heading-edit-engine)
               (list :match "/ABSENT" :tags "x" :at "2026-10-19T09:00:00"
                     :tz "UTC" :dst-policy "compatible")
               :tags-add)))
            :to-be :selection-no-match))
  (it "maps edit, resource, and generic Org kit errors to failures"
    (let ((engine (%heading-edit-engine)))
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-tags-edit
                 engine (%heading-edit-options :tags "bad tag") :tags-add)))
              :to-be :argument)
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-tags-edit
                 (%heading-edit-limited-engine)
                 (%heading-edit-options :tags "c") :tags-add)))
              :to-be :too-large)
      (%with-heading-edit-error
          org-kit:replace-heading-tags
          (make-condition 'org-kit:org-kit-error :message "tags failed")
        (expect (%heading-edit-failure-kind
                 (multiple-value-list
                  (aitools.org.infrastructure::%org-tags-edit
                   engine (%heading-edit-options :tags "c") :tags-replace)))
                :to-be :internal))))
  (it "does not swallow an unknown tag operation"
    (expect (handler-case
                (progn (aitools.org.infrastructure::%org-tags-edit
                        (%heading-edit-engine) (%heading-edit-options :tags "c")
                        :tags-bogus)
                       :returned)
              (type-error () :type-error))
            :to-be :type-error)))
