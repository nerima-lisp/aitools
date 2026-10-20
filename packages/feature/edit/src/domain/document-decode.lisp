(in-package #:aitools.edit.domain)

(aitools.kernel.domain:define-flow
  (decode-text-document/k octets &key on-decoded on-binary on-invalid)
  "Decode a file's OCTETS for a text edit and continue with ON-DECODED (DOCUMENT),
ON-BINARY () when the first 8 KiB hold a NUL, or ON-INVALID (OFFSET) at the
first byte that is not valid UTF-8."
  (let ((octets (coerce octets 'octets)))
    (if (aitools.text.domain:binary-octets-p octets)
        (aitools.kernel.domain:flow-continue
          :on-binary)
        (let ((start (aitools.text.domain:utf8-bom-length octets)))
          (aitools.text.domain:decode-utf8-strict/k
            octets
            :start
            start
            :on-decoded
            (lambda (text)
              (aitools.kernel.domain:flow-continue
                :on-decoded
                (make-text-document text :bom-p (plusp start))))
            :on-invalid
            (lambda (offset)
              (aitools.kernel.domain:flow-continue
                :on-invalid
                offset)))))))
