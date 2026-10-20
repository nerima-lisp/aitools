(in-package #:aitools.search.application)

(defconstant +read-chunk-size+ 65536)

(aitools.kernel.domain:define-flow
    (%read-disk-file/k source absolute &key on-text on-binary on-missing)
  "Read ABSOLUTE in chunks, checking the first chunk for binary content. Join
all text chunks into one octet vector."
  (let ((chunks '()) (total 0) (binary nil))
    (flet ((take (chunk)
             (if (and (null chunks) (aitools.text.domain:binary-octets-p chunk))
                 (progn (setf binary t) :stop)
                 (progn (push chunk chunks) (incf total (length chunk)) nil))))
      (declare (dynamic-extent #'take))
      (cond
        ((null (aitools.text.application:source-call-with-chunks
                source absolute +read-chunk-size+ #'take))
         (aitools.kernel.domain:flow-continue :on-missing))
        (binary (aitools.kernel.domain:flow-continue :on-binary))
        ((null chunks)
         (aitools.kernel.domain:flow-continue
          :on-text (make-array 0 :element-type '(unsigned-byte 8))))
        ((null (rest chunks)) (aitools.kernel.domain:flow-continue :on-text (first chunks)))
        (t (let ((octets (make-array total :element-type '(unsigned-byte 8))) (offset 0))
             (dolist (chunk (nreverse chunks))
               (replace octets chunk :start1 offset)
               (incf offset (length chunk)))
             (aitools.kernel.domain:flow-continue :on-text octets)))))))

(aitools.kernel.domain:define-flow
    (%read-file/k session absolute relative &key on-text on-binary on-missing)
  "Read staged or disk bytes and continue with the text, binary, or missing
result."
  (let ((staged (session-staged session)))
    (multiple-value-bind (state present) (and staged relative (gethash relative staged))
      (if present
          (let ((octets (and (eq (aitools.store.domain:entry-state-kind state) :file)
                             (aitools.store.application:view-read-file (session-view session) relative))))
            (cond ((null octets) (aitools.kernel.domain:flow-continue :on-missing))
                  ((aitools.text.domain:binary-octets-p octets)
                   (aitools.kernel.domain:flow-continue :on-binary))
                  (t (aitools.kernel.domain:flow-continue :on-text octets))))
          (aitools.kernel.domain:flow-call
              (%read-disk-file/k (search-ports-text-source (session-ports session)) absolute)
            (:on-text (octets) (aitools.kernel.domain:flow-continue :on-text octets))
            (:on-binary () (aitools.kernel.domain:flow-continue :on-binary))
            (:on-missing () (aitools.kernel.domain:flow-continue :on-missing)))))))
