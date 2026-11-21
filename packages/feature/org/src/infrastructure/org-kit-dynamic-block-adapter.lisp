(in-package #:aitools.org.infrastructure)

(defun edit-org-dynamic-block (engine options)
  (let* ((number (getf options :block)) (blocks nil)
         (agenda-documents (getf options :agenda-documents))
         (documents (cons engine agenda-documents)))
    (org-kit:map-nodes (lambda (node)
                         (when (eq (org-kit:node-type node) :dynamic-block)
                           (push node blocks)))
                       (org-kit:document-root engine))
    (let ((block (nth (1- number) (nreverse blocks))))
      (unless block
        (return-from edit-org-dynamic-block
          (values nil nil (%failure :argument "--block does not select a dynamic block"))))
      (multiple-value-bind (at context reference failure)
          (%explicit-time-values (getf options :at) (getf options :tz) (getf options :dst-policy))
        (declare (ignore at reference))
        (when failure (return-from edit-org-dynamic-block (values nil nil failure)))
        (handler-case
            (multiple-value-bind (result warnings)
                (%call-collecting-clock-warnings
                 (lambda ()
                   (org-kit:update-dynamic-block engine block
                    :document-resolver (lambda (selector)
                                         (when (eq selector :agenda) agenda-documents))
                    :time-context context)))
              (values (org-kit:edit-result-updated-document result)
                      (list (cons "block" number)
                            (cons "warnings" (%org-clock-warning-entries warnings documents)))
                      nil))
          (org-kit:org-clocktable-parameter-error ()
            (values nil nil (%failure :argument "Invalid or unsupported clocktable parameter")))
          (org-kit:org-dynamic-block-error ()
            (values nil nil (%failure :argument "Invalid or unsupported dynamic block")))
          (org-kit:org-resource-limit-error ()
            (values nil nil (%failure :too-large "Dynamic block update exceeded a resource limit")))
          (org-kit:org-kit-error ()
            (values nil nil (%failure :internal "Dynamic block update failed"))))))))

;;; A warning carries the CLOCK line number and text but not the document, so
;;; it is attributed to every aggregated document with that text on that line.
(defun %org-document-clock-at-p (document line source)
  (let ((found nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (and (eq (org-kit:node-type node) :clock)
                  (eql (org-kit:node-line-range node) line)
                  (search source (org-kit:node-text node)))
         (setf found t)))
     (org-kit:document-root document))
    found))

(defun %org-clock-warning-entries (warnings documents)
  (let ((entries nil))
    (dolist (warning warnings)
      (let ((line (org-kit:org-clock-warning-line warning))
            (source (org-kit:org-clock-warning-source warning)))
        (dolist (document documents)
          (when (%org-document-clock-at-p document line source)
            (pushnew (list (org-kit:document-source-name document) line source) entries
                     :test #'equal)))))
    (map 'vector (lambda (entry) (apply #'%org-clock-warning-object entry))
         (nreverse entries))))
