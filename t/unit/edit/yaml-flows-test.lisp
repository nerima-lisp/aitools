;;;; YAML write command flows and warning fields.
(in-package #:aitools.edit.test)

(describe "aitools.edit YAML write argument errors"
  (it "rejects invalid arguments before reading or changing a file"
    (call-with-edit-workspace (lambda () (put "doc.yaml" (format nil "keep: 1~%"))
      (let ((reads 0)
            (before (snapshot))
            (read-file
              (symbol-function 'aitools.store.application:view-read-file)))
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.store.application:view-read-file)
              (lambda (view path &rest arguments)
                (when (string= path "doc.yaml")
                  (incf reads))
                (apply read-file view path arguments))))
          (labels ((expect-error (expected-code command positionals &rest options)
                     (multiple-value-bind (status code message keys)
                         (apply (function run) command positionals options)
                       (declare (ignore message keys))
                       (expect status :to-be :error)
                       (expect code :to-equal expected-code))))
            (expect-error "argument.invalid" "doc.set" nil)
            (expect-error "argument.invalid" "doc.set" (list "doc.yaml" "/x")
                          :stdin-data "{\"pointer\":\"/x\",\"value\":1}")
            (expect-error "argument.invalid" "doc.set" (list "doc.yaml")
                          :stdin-data "{\"pointer\":\"/x\"}")
            (expect-error "argument.invalid" "doc.set" (list "doc.yaml")
                          :stdin-data "{\"pointer\":1,\"value\":1}")
            (expect-error "argument.invalid" "doc.set" (list "doc.yaml" "/x"))
            (expect-error "argument.invalid" "doc.set" (list "doc.yaml" "/x~2" "1"))
            (expect-error "argument.invalid" "doc.set" (list "doc.yaml" "/x" "bare"))
            (expect-error "argument.invalid" "doc.delete" (list "doc.yaml"))
            (expect-error "argument.invalid" "doc.delete" (list "doc.yaml" "/x~2"))
            (expect-error "argument.invalid" "doc.merge" (list "doc.yaml"))
            (expect-error "input.syntax-error" "doc.merge" (list "doc.yaml") :stdin-data "{")
            (expect-error "argument.invalid" "doc.patch" (list "doc.yaml" "extra")
                          :stdin-data "[]")
            (expect-error "argument.invalid" "doc.patch" (list "doc.yaml"))
            (expect-error "argument.invalid" "doc.fmt" nil)
            (expect reads :to-be 0)
            (expect (snapshot) :to-equal before))))))))
(describe "aitools.edit YAML write input limits"
  (it "accepts a document at the byte limit and rejects one byte over"
    (call-with-edit-workspace (lambda () (let* ((limit aitools.edit.application::+max-format-document-bytes+)
             (tail (format nil "keep: 1~%"))
             (at-limit-path "at-limit.yaml")
             (over-limit-path "over-limit.yaml")
             (document (byte-limit-comment-document limit tail)))
        (put at-limit-path document)
        (call-with-format-byte-limit-reads
         at-limit-path over-limit-path limit (bytes tail)
         (lambda ()
           (multiple-value-bind (status fields)
               (run "doc.fmt" (list at-limit-path) :dry-run t)
             (declare (ignore fields))
             (expect status :to-be :ok))
           (vector-push-extend (char-code #\x) document)
           (put over-limit-path document)
           (multiple-value-bind (status code message keys)
               (run "doc.fmt" (list over-limit-path) :dry-run t)
             (declare (ignore message keys))
             (expect status :to-be :error)
             (expect code :to-equal "refusal.too-large"))))))))
  (it "accepts the nesting limit and rejects one extra collection"
    (call-with-edit-workspace (lambda () (let ((limit aitools.edit.application::+max-format-document-depth+))
        (labels ((document-of (depth)
                   (concatenate 'string
                                (make-string depth :initial-element #\[)
                                "0"
                                (make-string depth :initial-element #\])
                                (format nil "~%"))))
          (put "at-depth.yaml" (document-of limit))
          (multiple-value-bind (status fields)
              (run "doc.fmt" (list "at-depth.yaml") :dry-run t)
            (declare (ignore fields))
            (expect status :to-be :ok))
          (put "over-depth.yaml" (document-of (1+ limit)))
          (multiple-value-bind (status code message keys)
              (run "doc.fmt" (list "over-depth.yaml") :dry-run t)
            (declare (ignore message keys))
            (expect status :to-be :error)
            (expect code :to-equal "input.syntax-error"))))))))
(describe "aitools.edit YAML write byte-limit errors"
  (it "does not infer byte-limit errors from their wording"
    (call-with-edit-workspace (lambda () (put "doc.yaml" (format nil "keep: 1~%"))
      (let ((read-file
              (symbol-function 'aitools.store.application:view-read-file)))
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.store.application:view-read-file)
              (lambda (view path &rest arguments)
                (if (string= path "doc.yaml")
                    (error 'aitools.store.application:store-io-error
                           :operation "read"
                           :path path
                           :detail "file exceeds byte limit")
                    (apply read-file view path arguments)))))
          (multiple-value-bind (status code message keys)
              (run "doc.fmt" (list "doc.yaml") :dry-run t)
            (declare (ignore message keys))
            (expect status :to-be :error)
            (expect code :to-equal "environment.io")))))))
  (it "does not treat downstream byte-limit errors as oversized input"
    (call-with-edit-workspace (lambda () (put "doc.yaml" (format nil "keep: 1~%"))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.inspect.application:parse-yaml-write-document/k)
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (error 'aitools.store.application:store-byte-limit-error
                     :operation "write"
                     :path "doc.yaml"
                     :detail "downstream failure"))))
        (multiple-value-bind (status code message keys)
            (run "doc.fmt" (list "doc.yaml") :dry-run t)
          (declare (ignore message keys))
          (expect status :to-be :error)
          (expect code :to-equal "environment.io")))))))
