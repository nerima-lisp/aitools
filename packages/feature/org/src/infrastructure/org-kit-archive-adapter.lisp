(in-package #:aitools.org.infrastructure)

(defun %archive-target (engine title) (let ((matches nil)) (org-kit:map-nodes (lambda (node) (when (and (eq (org-kit:node-type node) :headline) (string= (org-kit:node-property node :raw-value "") title)) (push node matches))) (org-kit:document-root engine)) (nreverse matches)))

(defun archive-org-documents (source destination options same-p)
  (multiple-value-bind (heading at context failure) (%org-selected-heading source options)
    (declare (ignore at))
    (when failure (return-from archive-org-documents (values nil nil failure)))
    (let* ((title (getf options :target-heading))
           (path (mapcar (lambda (entry)
                           (aitools.org.domain:make-org-heading-path-entry
                            :title (getf entry :title) :index (getf entry :index)))
                         (org-kit:node-heading-path heading))))
      (handler-case
          (let* ((timestamp (%archive-timestamp context (getf options :tz)))
                 (updated-source (org-kit:set-property source heading "ARCHIVE_TIME" timestamp))
                 (updated-target (if same-p updated-source destination))
                 (matches (%archive-target updated-target title)))
            (when (cdr matches)
              (return-from archive-org-documents (values nil nil (%failure :selection-ambiguous "archive target heading is ambiguous"))))
            (unless matches
              (let* ((text (org-kit:document-text updated-target))
                     (suffix (if (or (zerop (length text)) (char= (char text (1- (length text))) #\Newline)) "" (string #\Newline)))
                     (new-text (format nil "~A~A* ~A~%" text suffix title)))
                (setf updated-target (org-kit:parse-org new-text :source-name (org-kit:document-source-name updated-target) :limits *org-kit-limits*))
                (when same-p (setf updated-source updated-target))
                (setf matches (%archive-target updated-target title))))
            (let* ((relocation (org-kit:archive-subtree-to updated-source (%heading-at-path updated-source path) updated-target :target-heading (first matches)))
                   (source-result (org-kit:edit-result-updated-document (org-kit:relocation-result-source-edit relocation)))
                   (target-result (org-kit:edit-result-updated-document (org-kit:relocation-result-target-edit relocation))))
              (values source-result target-result nil)))
        (org-kit:org-edit-error (condition)
          (values nil nil
                  (if (eq (org-kit:org-edit-error-reason condition) :uneditable-structure)
                      (%failure :unsupported (org-kit:org-kit-error-message condition))
                      (%org-edit-condition-failure condition))))
        (org-kit:org-resource-limit-error () (values nil nil (%failure :too-large "archive exceeded a resource limit")))
        (org-kit:org-kit-error () (values nil nil (%failure :internal "archive edit failed")))))))

(defun %archive-timestamp (context zone)
  (aitools.org.domain:format-org-archive-timestamp
   (org-kit:org-time-context-reference-instant context) zone))
