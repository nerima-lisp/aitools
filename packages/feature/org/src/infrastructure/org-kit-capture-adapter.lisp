(in-package #:aitools.org.infrastructure)

(defun %capture-headlines (document)
  (let (headings)
    (org-kit:map-nodes (lambda (node)
                         (when (eq (org-kit:node-type node) :headline)
                           (push node headings)))
                       (org-kit:document-root document))
    (nreverse headings)))

(defun %capture-child (document parent title)
  (find title (%child-headlines (or parent (org-kit:document-root document)))
        :key (lambda (node) (org-kit:node-property node :raw-value ""))
        :test #'string=))

(defun %capture-path-node (document titles)
  (loop with parent = nil
        for title in titles
        do (setf parent (%capture-child document parent title))
        unless parent return nil
        finally (return parent)))

(defun %capture-date-titles (document result)
  (let* ((offset (org-kit:capture-target-result-cursor-offset result))
         (heading (find offset (%capture-headlines document)
                        :key #'org-kit:node-post-affiliated)))
    (loop for node = (and heading (org-kit:node-parent heading))
          then (org-kit:node-parent node)
          while (and node (eq (org-kit:node-type node) :headline))
          collect (org-kit:node-property node :raw-value "") into titles
          finally (return (nreverse titles)))))

(defun %capture-date-key (heading length)
  (let ((title (org-kit:node-property heading :raw-value "")))
    (when (and (>= (length title) length)
               (loop for i below length
                     always (if (member i '(4 7))
                                (char= (char title i) #\-)
                                (char<= #\0 (char title i) #\9)))
               (or (= (length title) length)
                   (char= (char title length) #\Space)))
      (subseq title 0 length))))

(defun %capture-order-new-path (before after titles)
  (loop for depth from 1 to 3
        for path = (subseq titles 0 depth)
        unless (%capture-path-node before path)
          do (let* ((parent (and (> depth 1)
                                 (%capture-path-node after (butlast path))))
                    (heading (%capture-path-node after path))
                    (siblings (%child-headlines (or parent (org-kit:document-root after))))
                    (key (%capture-date-key heading (nth (1- depth) '(4 7 10))))
                    (index (position heading siblings))
                    (first-later
                      (position-if (lambda (sibling)
                                     (let ((other (%capture-date-key sibling (length key))))
                                       (and other (string> other key))))
                                   siblings :end index)))
               (when first-later
                 (dotimes (i (- index first-later))
                   (declare (ignore i))
                   (setf after
                         (org-kit:edit-result-updated-document
                          (org-kit:move-subtree after (%capture-path-node after path)
                                                :direction :up)))))))
  after)

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
                 (let* ((result (org-kit:capture-into-target engine (list :file+olp+datetree path)
                                                             template :time-context context :limits *org-kit-limits*))
                        (updated (org-kit:capture-target-result-target-document result))
                        (titles (%capture-date-titles updated result)))
                   (values (%capture-order-new-path engine updated titles) nil nil)))
                (t (values nil nil (%failure :argument "--target must be headline or datetree")))))))
      (org-kit:org-resource-limit-error ()
        (values nil nil (%failure :too-large "Org capture exceeded a resource limit")))
      (org-kit:org-edit-error (condition)
        (values nil nil (%org-edit-condition-failure condition)))
      (org-kit:org-kit-error ()
        (values nil nil (%failure :internal "Org capture failed"))))))
