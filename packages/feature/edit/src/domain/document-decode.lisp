(in-package #:aitools.edit.domain)

(defun decode-text-document/k (octets &key on-decoded on-binary on-invalid)
  "Decode a file's OCTETS for a text edit and call exactly one of
ON-DECODED (document), ON-BINARY () when the first 8 KiB hold a NUL, or
ON-INVALID (offset) at the first byte that is not valid UTF-8."
  (declare (type function on-decoded on-binary on-invalid))
  (let ((octets (coerce octets 'octets)))
    (if (aitools.text.domain:binary-octets-p octets)
        (funcall on-binary)
        (let ((start (aitools.text.domain:utf8-bom-length octets)))
          (aitools.text.domain:decode-utf8-strict/k
           octets :start start
           :on-decoded (lambda (text) (funcall on-decoded (make-text-document text :bom-p (plusp start))))
           :on-invalid on-invalid)))))
