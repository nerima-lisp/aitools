(in-package #:aitools.org.infrastructure)

(defparameter *org-kit-limits* (org-kit:make-limits))

(defun %failure (kind message &key line column offset)
  (aitools.org.domain:make-org-failure
   :kind kind :message message :line line :column column :offset offset))

(defun %resource-failure (condition)
  (%failure :too-large (org-kit:org-kit-error-message condition)))

(defun parse-org-octets (octets &key source-name)
  (let ((bytes (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (handler-case
        (values (aitools.org.domain:make-org-document
                 source-name bytes
                 (org-kit:parse-org bytes :source-name source-name :encoding :utf-8
                                    :limits *org-kit-limits*))
                nil)
      (org-kit:org-resource-limit-error (condition)
        (values nil (%resource-failure condition)))
      (org-kit:org-parse-error (condition)
        (let* ((message (org-kit:org-kit-error-message condition))
               (invalid-utf8-p (string= message "Input is not validly encoded")))
          (values nil
                  (%failure (if invalid-utf8-p :invalid-utf8 :syntax)
                            (if invalid-utf8-p "Org input is not valid UTF-8" message)
                            :line (org-kit:org-parse-error-line condition)
                            :column (org-kit:org-parse-error-column condition)
                            :offset (org-kit:org-parse-error-offset condition)))))
      (org-kit:org-kit-error (condition)
        (values nil (%failure :internal
                              (org-kit:org-kit-error-message condition)))))))

(defun %outline-entry (entry)
  (aitools.org.domain:make-org-outline-entry
   :kind (org-kit:outline-entry-kind entry)
   :name (or (org-kit:outline-entry-name entry) "")
   :level (org-kit:outline-entry-level entry)
   :line (org-kit:outline-entry-line entry)))

(defun outline-org-document (document limit)
  (handler-case
      (let* ((result (org-kit:outline
                      (aitools.org.domain:org-document-engine-value document)
                      :limit limit))
             (entries (mapcar (function %outline-entry)
                              (org-kit:outline-result-symbols result))))
        (values (list :entries entries
                      :total (org-kit:outline-result-total result)
                      :truncated (org-kit:outline-result-truncated result))
                nil))
    (org-kit:org-resource-limit-error (condition)
      (values nil (%resource-failure condition)))
    (org-kit:org-kit-error (condition)
      (values nil (%failure :internal (org-kit:org-kit-error-message condition))))))

(defun %link-type-name (value)
  (typecase value
    (string (string-downcase value))
    (symbol (string-downcase (symbol-name value)))
    (t nil)))

(defun %export-link-failure (document)
  (let ((found nil)
        (root (org-kit:document-root
               (aitools.org.domain:org-document-engine-value document))))
    (when root
      (org-kit:map-nodes
       (lambda (node)
         (when (and (null found) (eq (org-kit:node-type node) :link))
           (let ((kind (%link-type-name (org-kit:node-property node :type))))
             (unless (member kind '("http" "https" "mailto")
                             :test #'string=)
               (setf found (list node kind))))))
       root))
    (when found
      (let* ((node (first found))
             (kind (or (second found) "unknown")))
        (multiple-value-bind (line column) (org-kit:node-start-position node)
          (%failure :internal-link
                    (format nil "export does not support Org link type ~A" kind)
                    :line line :column column))))))

(defun export-org-document (document backend)
  (let ((link-failure (%export-link-failure document)))
    (if link-failure
        (values nil link-failure)
        (handler-case
            (values (org-kit:export-org
                     (aitools.org.domain:org-document-engine-value document) backend)
                    nil)
          (org-kit:org-resource-limit-error (condition)
            (values nil (%resource-failure condition)))
          (org-kit:org-export-unsupported-syntax (condition)
            (let ((position (org-kit:org-export-unsupported-syntax-position condition))
                  (source (aitools.org.domain:org-document-engine-value document)))
              (if position
                  (multiple-value-bind (line column)
                      (org-kit:offset-line-column source position)
                    (values nil (%failure :unsupported
                                          (org-kit:org-kit-error-message condition)
                                          :line line :column column :offset position)))
                  (values nil (%failure :unsupported
                                        (org-kit:org-kit-error-message condition))))))
          (org-kit:org-export-error (condition)
            (values nil (%failure :unsupported (org-kit:org-kit-error-message condition))))
          (org-kit:org-kit-error (condition)
            (values nil (%failure :internal (org-kit:org-kit-error-message condition))))))))
