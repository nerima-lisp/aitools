(in-package #:aitools.md.infrastructure)

(defun %utf8-width (character)
  (let ((code (char-code character)))
    (cond
      ((<= code #x7f) 1)
      ((<= code #x7ff) 2)
      ((<= code #xffff) 3)
      (t 4))))

(defun %section-byte-offsets (source heading)
  (let ((start-char (aitools.md.domain:md-heading-start-char heading))
        (end-char (aitools.md.domain:md-heading-end-char heading))
        (byte-offset 0)
        (start-byte nil))
    (loop for index from 0 to end-char
          do (when (= index start-char)
               (setf start-byte byte-offset)) (when (< index end-char)
                                                (incf byte-offset (%utf8-width (char source index)))))
    (values start-byte byte-offset)))

(defun section-bytes (document heading)
  (multiple-value-bind (start end) (%section-byte-offsets (aitools.md.domain:md-document-source document) heading)
    (subseq (aitools.md.domain:md-document-octets document) start end)))

(defun splice-section-bytes (document heading replacement)
  (multiple-value-bind (start end) (%section-byte-offsets (aitools.md.domain:md-document-source document) heading)
    (let* ((original (aitools.md.domain:md-document-octets document))
           (replacement-length (length replacement))
           (result (make-array (+ start replacement-length (- (length original) end)) :element-type '(unsigned-byte 8))))
      (replace result original :start1 0 :end1 start)
      (replace result replacement :start1 start)
      (replace result original :start1 (+ start replacement-length) :start2 end)
      result)))

(defun %parsed-headings (engine-document)
  (let ((section 0))
    (loop for record in (cl-markdown-kit:document-headings engine-document)
          for span = (cl-markdown-kit:heading-record-section-span record)
          when span
            collect (progn
                      (incf section)
                      (let ((node-span (cl-markdown-kit:node-span (cl-markdown-kit:heading-record-node record))))
                        (aitools.md.domain:make-md-heading section
                                                           (cl-markdown-kit:heading-record-plain-text record)
                                                           (cl-markdown-kit:heading-record-level record)
                                                           (cl-parser-kit:span-start-line node-span)
                                                           (cl-parser-kit:span-start span)
                                                           (cl-parser-kit:span-end span)))))))

(defun parse-md-source (source octets)
  (handler-case
      (let* ((engine-document
               (cl-markdown-kit:parse-string source
                                             :front-matter t
                                             :gfm t
                                             :max-input-size aitools.md.application:+md-max-input-bytes+))
             (headings (%parsed-headings engine-document)))
        (values (aitools.md.domain:make-md-document source octets headings) nil nil nil))
    (cl-markdown-kit:markdown-resource-limit-error (condition)
      (values nil "refusal.too-large" (cl-markdown-kit:markdown-kit-error-message condition) nil))
    (cl-markdown-kit:markdown-kit-error (condition)
      (values nil "input.syntax-error" (cl-markdown-kit:markdown-kit-error-message condition) nil))))
