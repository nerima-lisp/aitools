(in-package #:aitools.org.infrastructure)

(defparameter *org-kit-limits* (org-kit:make-limits))

(defun %failure (kind message &key line column offset)
  (aitools.org.domain:make-org-failure
   :kind kind :message message :line line :column column :offset offset))

(defun %resource-failure (condition)
  (%failure :too-large (org-kit:org-kit-error-message condition)))

(defun %utf16-or-32-bom (octets)
  (let ((length (length octets)))
    (cond
      ((and (>= length 4) (equalp (subseq octets 0 4) #(255 254 0 0))) t)
      ((and (>= length 4) (equalp (subseq octets 0 4) #(0 0 254 255))) t)
      ((and (>= length 2) (equalp (subseq octets 0 2) #(255 254))) t)
      ((and (>= length 2) (equalp (subseq octets 0 2) #(254 255))) t))))

(defun %invalid-utf8-offset (octets)
  (let ((offset nil))
    (aitools.text.domain:decode-utf8-strict/k
     octets
     :on-decoded (lambda (string) (declare (ignore string)) nil)
     :on-invalid (lambda (position) (setf offset position)))
    offset))

(defun parse-org-octets (octets &key source-name)
  (let ((bytes (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (cond
      ((%utf16-or-32-bom bytes)
       (values nil (%failure :unsupported "UTF-16 and UTF-32 Org input is unsupported")))
      (t
       (let ((offset (%invalid-utf8-offset bytes)))
         (cond
           ((integerp offset)
            (values nil (%failure :invalid-utf8 "Org input is not valid UTF-8"
                                  :offset offset)))
           ((find 0 bytes)
            (values nil (%failure :unsupported "Org input contains a NUL byte")))
           (t
            (handler-case
                (values (aitools.org.domain:make-org-document
                         source-name bytes
                         (org-kit:parse-org bytes :source-name source-name :encoding :utf-8
                                            :limits *org-kit-limits*))
                        nil)
              (org-kit:org-resource-limit-error (condition)
                (values nil (%resource-failure condition)))
              (org-kit:org-parse-error (condition)
                (values nil (%failure :syntax (org-kit:org-kit-error-message condition)
                                      :line (org-kit:org-parse-error-line condition)
                                      :column (org-kit:org-parse-error-column condition)
                                      :offset (org-kit:org-parse-error-offset condition))))
              (org-kit:org-kit-error (condition)
                (values nil (%failure :syntax (org-kit:org-kit-error-message condition))))
              (type-error (condition)
                (values nil (%failure :argument (princ-to-string condition))))))))))))

(defun %outline-entry (entry)
  (aitools.org.domain:make-org-outline-entry
   :kind (org-kit:outline-entry-kind entry)
   :name (or (org-kit:outline-entry-name entry) "")
   :level (org-kit:outline-entry-level entry)
   :line (org-kit:outline-entry-line entry)))

(defun outline-org-document (document limit)
  (if (and limit (not (typep limit '(integer 0 *))))
      (values nil (%failure :argument "outline limit must be a non-negative integer"))
      (handler-case
          (let* ((result (org-kit:outline
                          (aitools.org.domain:org-document-engine-value document)
                          :limit limit))
                 (entries (mapcar #'%outline-entry
                                  (org-kit:outline-result-symbols result))))
            (values (list :entries entries
                          :total (org-kit:outline-result-total result)
                          :truncated (org-kit:outline-result-truncated result))
                    nil))
        (org-kit:org-resource-limit-error (condition)
          (values nil (%resource-failure condition)))
        (org-kit:org-kit-error (condition)
          (values nil (%failure :syntax (org-kit:org-kit-error-message condition))))
        (type-error (condition)
          (values nil (%failure :argument (princ-to-string condition)))))))

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
               (setf found (list node kind)))))
       root))
    (when found
      (let* ((node (first found))
             (kind (or (second found) "unknown")))
        (multiple-value-bind (line column) (org-kit:node-start-position node)
          (%failure :internal-link
                    (format nil "export does not support Org link type ~A" kind)
                    :line line :column column)))))))

(defun export-org-document (document backend)
  (let ((link-failure (%export-link-failure document)))
    (cond
      ((not (member backend '(:html :gfm :ascii)))
       (values nil (%failure :argument "export backend must be html, gfm, or ascii")))
      (link-failure (values nil link-failure))
      (t
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
           (values nil (%failure :unsupported (org-kit:org-kit-error-message condition))))))))
)
