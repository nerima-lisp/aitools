(in-package #:aitools.edit.application)

(define-write-command "split" (ports env positionals options &key on-plan on-error)
  (let* ((path (first positionals))
         (methods
          (remove nil
                  (list (and (getf options :lines) :lines)
                        (and (getf options :at-match) :at-match)
                        (and (getf options :bytes) :bytes))))
         (digits (parse-count (or (getf options :suffix-digits) "3"))))
    (cond
      ((or (null path) (rest positionals)) (flow-continue :on-error "argument.invalid" "split takes exactly one path"))
      ((/= (length methods) 1)
       (flow-continue :on-error "argument.invalid" "split needs exactly one of --lines, --at-match, --bytes"))
      ((not (and digits (<= 1 digits 9))) (flow-continue :on-error "argument.invalid" "--suffix-digits must be 1 to 9"))
      (t
       (let ((method (first methods))
             (prefix (or (getf options :prefix) (concatenate 'string path "."))))
         (flet ((plan (piece-function)
                  (flow-continue :on-plan
                                 (make-write-plan :command
                                                  "split"
                                                  :targets
                                                  (list (make-write-target path)
                                                        (make-write-target (split-piece-name prefix 1 digits)))
                                                  :plan
                                                  (lambda (context &key on-committed on-rejected)
                                                    (aitools.kernel.domain:flow-call
                                                      (%split-plan context piece-function prefix digits)
                                                      (:on-error
                                                          (code message
                                                                &key
                                                                candidates
                                                                diagnostics
                                                                conflicts
                                                                repairs
                                                                path)
                                                        (declare (ignore path))
                                                        (funcall on-rejected
                                                                 code
                                                                 message
                                                                 :candidates
                                                                 candidates
                                                                 :diagnostics
                                                                 diagnostics
                                                                 :conflicts
                                                                 conflicts
                                                                 :repairs
                                                                 repairs))
                                                      (:on-committed (requests &optional extra)
                                                        (funcall on-committed requests extra))))
                                                  :record-options
                                                  options
                                                  :record-positionals
                                                  (lambda (paths)
                                                    (list (first paths)))))))
           (ecase method
             ((:lines :bytes)
              (let ((count (parse-count (getf options method))))
                (if (and count (plusp count))
                    (plan
                     (lambda (octets document)
                       (declare (ignore document))
                       (if (eq method :lines)
                           (split-by-lines octets count)
                           (split-by-bytes octets count))))
                    (flow-continue :on-error
                                   "argument.invalid"
                                   (format
                                     nil
                                     "--~(~A~) must be a positive integer"
                                     method)))))
             (:at-match
              (aitools.kernel.domain:flow-call
                (compile-pattern/k (getf options :at-match))
                (:on-error (code message)
                  (flow-continue :on-error code message))
                (:on-regex (regex)
                  (plan
                   (lambda (octets document)
                     (split-at-matches octets (text-document-lines document) regex)))))))))))))
