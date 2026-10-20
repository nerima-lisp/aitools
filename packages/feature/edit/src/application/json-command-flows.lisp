(in-package #:aitools.edit.application)

(define-write-command "json.merge" (ports env positionals options on-plan fail)
  (%json-stdin-command "json.merge" ports positionals options on-plan fail
                       (lambda (patch) (lambda (document) (json-merge-patch document patch)))))

(define-write-command "json.patch" (ports env positionals options on-plan fail)
  (%json-stdin-command "json.patch" ports positionals options on-plan fail
                       (lambda (operations) (lambda (document) (json-apply-patch document operations)))))

(define-write-command "json.fmt" (ports env positionals options on-plan fail)
                      (let ((indent (getf options :indent)))
                        (cond
                         ((/= (length positionals) 1) (funcall fail "argument.invalid"
                                                               "json fmt takes exactly one PATH"))
                         ((and indent (not (and (parse-count indent) (<= (parse-count indent) 16))))
                          (funcall fail "argument.invalid" (format nil "--indent ~S must be 0 to 16" indent)))
                         ((and indent (getf options :minify)) (funcall fail "argument.invalid"
                                                                       "--indent and --minify cannot be combined"))
                         (t
                          (funcall on-plan
                                   (make-write-plan
                                    :command "json.fmt"
                                    :targets (list (make-write-target (first positionals)))
                                    :expect-hashes (getf options :expect-hash)
                                    :replayable (null (getf options :expect-hash))
                                    :plan (%json-plan #'identity
                                                      :sort-keys (getf options :sort-keys)
                                                      :indent-function (lambda (detected)
                                                                         (cond ((getf options :minify) nil)
                                                                               (indent (make-string (parse-count
                                                                                                     indent)
                                                                                                    :initial-element
                                                                                                    #\Space))
                                                                               (t (or detected "  ")))))
                                    :record-options options
                                    :record-positionals (lambda (paths) (list (first paths)))))))))
