(in-package #:cl-user)

(defpackage #:aitools.util.test
  (:use #:cl #:aitools.util.domain)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:it-property #:gen-integer #:gen-list #:expect #:signals
                #:make-mock-function #:mock-calls)
  (:import-from #:aitools.test.support #:string-bytes)
  (:import-from #:aitools.util.application
                #:make-input-request #:util-encode-flow #:util-decode-flow #:util-redact-flow
                #:util-tokens-flow #:util-calc-flow #:util-uuid-flow #:util-random-flow))

(in-package #:aitools.util.test)

(defun octets (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8) :initial-contents values))

(defun calc (text)
  "(VALUES :VALUE rational) or (VALUES :ERROR reason) for TEXT."
  (evaluate-expression text
                         :on-value (lambda (value) (values :value value))
                         :on-error (lambda (offset reason) (declare (ignore offset)) (values :error reason))))

(defun decode (scheme text)
  "(VALUES :DECODED octets) or (VALUES :INVALID offset) for TEXT's bytes."
  (decode-text scheme (string-bytes text)
                 :on-decoded (lambda (octets) (values :decoded octets))
                 :on-invalid (lambda (offset reason) (declare (ignore reason)) (values :invalid offset))))

(defun unused-reader (name)
  (make-mock-function
   (lambda (&rest arguments)
     (declare (ignore arguments))
     (error "reader ~A was called unexpectedly" name))))

(defun test-boundaries
    (&key (random-source (cl-boundary-kit:make-deterministic-random-source :seed 7))
          (uuid-source (cl-boundary-kit:make-test-uuid-source))
          (clock (cl-boundary-kit:make-fake-clock :start 1700000000000))
          (read-file-octets (unused-reader "read-file-octets"))
          (read-stdin-octets (unused-reader "read-stdin-octets")))
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
               (list
                (cons :util-random-octets
                      (lambda (count)
                        (cl-boundary-kit:random-source-bytes random-source count)))
                (cons :util-uuid-v4
                      (lambda () (cl-boundary-kit:uuid-generate uuid-source)))
                (cons :util-unix-ms
                      (lambda () (cl-boundary-kit:clock-now clock)))
                (cons :util-read-file-octets read-file-octets)
                (cons :util-read-stdin-octets read-stdin-octets)
                (cons :util-workspace-host nil)
                (cons :util-open-store (unused-reader "open-store"))))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(defun stdin-reader (text)
  (make-mock-function
   (lambda (limit)
     (let ((octets (string-bytes text)))
       (if (> (length octets) limit)
           (values :too-large)
           (values :octets octets))))))

(defun run-flow (function &rest arguments)
  "Run a util flow under CALL-WITH-COMMAND-RESULT/K; return (VALUES KIND
FIELDS) where FIELDS is the ok alist or the error plist."
  (let ((result (aitools.protocol.application:call-with-command-result/k
                 (lambda (&key on-ok on-partial on-error)
                   (declare (ignore on-partial))
                   (apply function (append arguments (list :on-ok on-ok :on-error on-error)))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun field (alist name)
  (cdr (assoc name alist :test #'string=)))

(defun uuid-version (uuid) (digit-char-p (char uuid 14) 16))

(defun uuid-variant-bits (uuid) (ash (digit-char-p (char uuid 19) 16) -2))

(defun uuid-v7-ms (uuid)
  (parse-integer (remove #\- (subseq uuid 0 13)) :radix 16))
