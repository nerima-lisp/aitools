(in-package #:aitools.org.infrastructure)

;;; The echoed name is cut at the same length cl-org-kit uses for the values
;;; in its own condition messages.
(defparameter *clocktable-scope-name-limit* 80)

(defun %unsupported-clocktable-scope-message (selector)
  (let ((name (if (stringp selector)
                  selector
                  (string-downcase (symbol-name selector)))))
    (format nil "Unsupported clocktable scope ~A; use file, agenda, subtree, tree, treeN (N is 1 or more), or nil (the default)"
            (if (> (length name) *clocktable-scope-name-limit*)
                (concatenate 'string (subseq name 0 *clocktable-scope-name-limit*) "...")
                name))))

(defun edit-org-dynamic-block (engine options)
  (let ((number (getf options :block)) (blocks nil) (agenda-documents nil) (agenda-failure nil))
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
        (let ((outcome
                (handler-case
                    (multiple-value-bind (result warnings)
                        (%call-collecting-clock-warnings
                         (lambda ()
                           (org-kit:update-dynamic-block engine block
                            :document-resolver
                            (lambda (selector)
                              (cond
                                ((eq selector :agenda)
                                 (multiple-value-setq (agenda-documents agenda-failure)
                                   (funcall (getf options :agenda-loader)))
                                 (unless (or agenda-failure agenda-documents)
                                   (setf agenda-failure
                                         (%failure :argument
                                                   (if (getf options :agenda-glob)
                                                       "--agenda-glob matched no Org files for :scope agenda"
                                                       "--agenda-glob is required for :scope agenda"))))
                                 agenda-documents)
                                (t
                                 (setf agenda-failure
                                       (%failure :argument
                                                 (%unsupported-clocktable-scope-message selector)))
                                 nil)))
                            :time-context context)))
                      (list :updated result warnings))
                  (org-kit:org-clocktable-parameter-error (condition)
                    (list :failed (%failure :argument (princ-to-string condition))))
                  (org-kit:org-dynamic-block-error (condition)
                    (list :failed (%failure :argument (princ-to-string condition))))
                  (org-kit:org-resource-limit-error (condition)
                    (list :failed (%failure :too-large (princ-to-string condition))))
                  (org-kit:org-kit-error (condition)
                    (list :failed (%failure :internal (princ-to-string condition)))))))
          (cond
            (agenda-failure (values nil nil agenda-failure))
            ((eq (first outcome) :failed) (values nil nil (second outcome)))
            (t (destructuring-bind (result warnings) (rest outcome)
                 (values (org-kit:edit-result-updated-document result)
                         (list (cons "block" number)
                               (cons "warnings"
                                     (%org-clock-warning-entries
                                      warnings (cons engine agenda-documents))))
                         nil)))))))))

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
