;;;; t/unit/util/package.lisp
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

(defun unused-port (name)
  (make-mock-function
   (lambda (&rest arguments)
     (declare (ignore arguments))
     (error "port ~A was called but must not be" name))))

(defun test-ports (&key (random-source (cl-boundary-kit:make-deterministic-random-source :seed 7))
                     (uuid-source (cl-boundary-kit:make-test-uuid-source))
                     (clock (cl-boundary-kit:make-fake-clock :start 1700000000000))
                     (read-file-octets (unused-port "read-file-octets"))
                     (read-stdin-octets (unused-port "read-stdin-octets")))
  "UTIL-PORTS over deterministic cl-boundary-kit fakes. Input reader ports
default to fail-fast cl-weave mocks."
  (aitools.util.infrastructure::make-util-ports-from-boundaries :random-source random-source :uuid-source uuid-source :clock clock
                                   :read-file-octets read-file-octets :read-stdin-octets read-stdin-octets))

(defun stdin-port (text)
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
