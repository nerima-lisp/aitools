(in-package #:aitools.md.application)

(defun %md-section-write (boundaries path section replacement octets)
  (if (null octets)
      (values nil nil "input.not-found" "Markdown file does not exist" nil "Pass an existing Markdown file.")
      (multiple-value-bind (document code message offset)
          (%md-parse-octets (cl-boundary-kit:boundary-context-require boundaries :md-parse-markdown) octets)
        (cond
          ((null document)
           (values nil nil code message
                   (and offset (list (json-object "offset" offset "message" message)))
                   "Check the Markdown input."))
          (t
           (let ((heading (aitools.md.domain:md-document-heading document section)))
             (if (null heading)
                 (values nil nil "selection.no-match"
                         (format nil "section ~D does not exist" section)
                         nil "Choose an ordinal from md headings.")
                 (let* ((updated (funcall (cl-boundary-kit:boundary-context-require boundaries :md-splice-section) document heading replacement))
                        (requests (if (equalp octets updated)
                                      nil
                                      (list (aitools.store.domain:write-file-request path updated)))))
                   (values requests
                           (list (cons "section" section)
                                 (cons "heading" (aitools.md.domain:md-heading-heading heading))
                                 (cons "level" (aitools.md.domain:md-heading-level heading))
                                 (cons "line" (aitools.md.domain:md-heading-line heading)))
                           nil nil nil nil))))))))
)
(defun %md-prepare-write (boundaries context section replacement)
  (let* ((view (aitools.edit.application:write-context-view context))
         (path (aitools.edit.application:context-path context))
         (state (aitools.store.application:view-path-kind view path)))
    (cond
      ((aitools.store.domain:entry-state-absent-p state)
       (values nil nil "input.not-found" "Markdown file does not exist" nil "Pass an existing Markdown file."))
      ((not (eq (aitools.store.domain:entry-state-kind state) :file))
       (values nil nil "refusal.not-a-file" "Markdown target is not a regular file" nil "Pass a regular Markdown file."))
      (t
       (handler-case
           (let ((octets (aitools.store.application:view-read-file view path :max-bytes +md-max-input-bytes+)))
             (%md-section-write boundaries path section replacement octets))
         (aitools.store.application:store-byte-limit-error ()
           (values nil nil "refusal.too-large"
                   "Markdown file exceeds the 16777216-byte input limit"
                   nil "Use a smaller Markdown file."))
         (aitools.store.application:store-io-error (condition)
           (values nil nil "environment.io" (princ-to-string condition)
                   nil "Check file permissions."))))))
)
(aitools.kernel.domain:define-flow
    (%md-plan-write/k boundaries context section replacement &key on-committed on-rejected)
  (declare (type function on-committed on-rejected))
  (multiple-value-bind (requests fields code message diagnostics repair)
      (%md-prepare-write boundaries context section replacement)
    (if code
        (aitools.kernel.domain:flow-continue
         :on-rejected code message
         :path (aitools.edit.application:context-path context)
         :diagnostics diagnostics
         :repairs (%md-schema-repair "md replace" repair))
        (aitools.kernel.domain:flow-continue :on-committed requests fields))))
