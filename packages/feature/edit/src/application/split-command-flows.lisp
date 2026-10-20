(in-package #:aitools.edit.application)

(define-write-command "split" (ports env positionals options on-plan fail)
                      (let* ((path (first positionals))
                             (methods (remove nil (list (and (getf options :lines) :lines) (and (getf options
                                                                                                      :at-match)
                                                                                                :at-match)
                                                        (and (getf options :bytes) :bytes))))
                             (digits (parse-count (or (getf options :suffix-digits) "3"))))
                        (cond
                         ((or (null path) (rest positionals)) (funcall fail "argument.invalid"
                                                                       "split takes exactly one path"))
                         ((/= (length methods) 1) (funcall fail "argument.invalid"
                                                           "split needs exactly one of --lines, --at-match, --bytes"))
                         ((not (and digits (<= 1 digits 9))) (funcall fail "argument.invalid"
                                                                      "--suffix-digits must be 1 to 9"))
                         (t
                          (let ((method (first methods))
                                (prefix (or (getf options :prefix) (concatenate 'string path "."))))
                            (flet ((plan (piece-function)
                                         (funcall on-plan
                                                  (make-write-plan
                                                   :command "split"
                                                   :targets (list (make-write-target path)
                                                                  (make-write-target (split-piece-name prefix 1
                                                                                                       digits)))
                                                   :plan (lambda (context commit reject)
                                                           (%split-plan context commit reject piece-function prefix
                                                                        digits))
                                                   :record-options options
                                                   :record-positionals (lambda (paths) (list (first paths)))))))
                              (ecase method
                                ((:lines :bytes)
                                 (let ((count (parse-count (getf options method))))
                                   (if (and count (plusp count))
                                       (plan (lambda (octets document) (declare (ignore document))
                                               (if (eq method :lines) (split-by-lines octets count) (split-by-bytes
                                                                                                     octets count))))
                                     (funcall fail "argument.invalid" (format nil "--~(~A~) must be a positive integer"
                                                                              method)))))
                                (:at-match
                                 (compile-pattern/k (getf options :at-match) fail
                                                    (lambda (regex)
                                                      (plan (lambda (octets document)
                                                              (split-at-matches octets (text-document-lines document)
                                                                                regex)))))))))))))
