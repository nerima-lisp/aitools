;;;; packages/core/text/src/domain/mime.lisp
;;;;
;;;; The `mime` field: magic signatures first (data/domain/text/
;;;; mime-data.lisp), then binary content is application/octet-stream, and
;;;; text is refined by extension or falls back to text/plain.
(in-package #:aitools.text.domain)

(defun %signature-matches-p (octets clauses)
  (every (lambda (clause)
           (destructuring-bind (offset &rest bytes) clause
             (and (<= (+ offset (length bytes)) (length octets))
                  (loop for byte in bytes for i from offset always (= (aref octets i) byte)))))
         clauses))

(defun %extension (path)
  (let* ((name (subseq path (1+ (or (position #\/ path :from-end t) -1))))
         (dot (position #\. name :from-end t)))
    (and dot (plusp dot) (string-downcase (subseq name (1+ dot))))))

(defun guess-mime (octets &key path)
  "The MIME type for a file whose leading bytes are OCTETS (at least the
first +BINARY-SNIFF-LENGTH+ bytes when available). PATH, when given,
refines the type of text content by its extension."
  (declare (type octets octets))
  (or (loop for (mime . clauses) in aitools.data:*text-mime-magic*
            when (%signature-matches-p octets clauses) return mime)
      (cond ((binary-octets-p octets) "application/octet-stream")
            ((and path (cdr (assoc (%extension path) aitools.data:*text-mime-extensions*
                                   :test #'equal))))
            (t "text/plain"))))
