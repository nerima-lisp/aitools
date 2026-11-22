(in-package #:aitools.org.infrastructure)

(defun %capture-headlines (document)
  (let (headings)
    (org-kit:map-nodes (lambda (node)
                         (when (eq (org-kit:node-type node) :headline)
                           (push node headings)))
                       (org-kit:document-root document))
    (nreverse headings)))

(defun %capture-body-invalid-p (engine title body)
  (or (not (and (stringp title) (plusp (length title))))
      (find #\Newline title)
      (find #\Return title)
      (and body
           (plusp (length (%capture-headlines
                           (org-kit:parse-org body :limits *org-kit-limits*
                                             :settings (org-kit:document-settings engine))))))))

(defun %org-capture-edit (engine options)
  (let ((title (getf options :title))
        (body (getf options :body))
        (target (getf options :target)))
    (handler-case
        (progn
          (when (%capture-body-invalid-p engine title body)
            (return-from %org-capture-edit
              (values nil nil (%failure :argument "--title must be a nonempty line and --body must not contain Org headings"))))
          (multiple-value-bind (at context reference failure)
              (%explicit-time-values (getf options :at) (getf options :tz) (getf options :dst-policy))
            (declare (ignore at))
            (when failure (return-from %org-capture-edit (values nil nil failure)))
            (let* ((template (org-kit:make-capture-template-result
                              :expanded-text (format nil "* ~A~%~A" title (or body ""))
                              :cursor-offset 0))
                   (path (org-kit:document-source-name engine)))
              (cond
                ((string= target "headline")
                 (let ((failure (%selector-failure options)))
                   (when failure (return-from %org-capture-edit (values nil nil failure))))
                 (multiple-value-bind (heading failure) (%org-select-heading-node engine options reference)
                   (when failure (return-from %org-capture-edit (values nil nil failure)))
                   ;; Capture resolves the first matching title and cannot select
                   ;; a later duplicate. Refile from a temporary document into the
                   ;; exact node selected by --match or --heading-path.
                   (let* ((scratch (org-kit:parse-org (format nil "* Capture target~%") :source-name "capture-scratch"
                                                       :limits *org-kit-limits*
                                                       :settings (org-kit:document-settings engine)))
                          (result (org-kit:capture-into-target
                                   scratch '(:file+headline "capture-scratch" "Capture target")
                                   template :time-context context :limits *org-kit-limits*))
                          (captured (org-kit:capture-target-result-target-document result))
                          (node (find (org-kit:capture-target-result-cursor-offset result)
                                      (%capture-headlines captured) :key #'org-kit:node-post-affiliated))
                          (moved (org-kit:refile-subtree captured node engine heading :mode :copy)))
                     (values (org-kit:edit-result-updated-document
                              (org-kit:relocation-result-target-edit moved)) nil nil))))
                ((string= target "datetree")
                 (when (or (getf options :match) (getf options :heading-path))
                   (return-from %org-capture-edit
                     (values nil nil (%failure :argument "datetree does not accept a heading selector"))))
                 (let ((result (org-kit:capture-into-target engine (list :file+olp+datetree path)
                                                            template :time-context context :limits *org-kit-limits*)))
                   (values (org-kit:capture-target-result-target-document result) nil nil)))
                (t (values nil nil (%failure :argument "--target must be headline or datetree")))))))
      (org-kit:org-resource-limit-error ()
        (values nil nil (%failure :too-large "Org capture exceeded a resource limit")))
      (org-kit:org-edit-error (condition)
        (values nil nil (%org-edit-condition-failure condition)))
      (org-kit:org-kit-error ()
        (values nil nil (%failure :internal "Org capture failed"))))))
