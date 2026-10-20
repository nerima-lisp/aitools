(in-package #:aitools.edit.application)

(define-write-command "replace" (ports env positionals options on-plan fail)
                      ;; A refusal without its own :PATH is about the first file searched, not the
                      ;; first positional (the pattern), so its default repair names a file.
                      (setf fail (let ((first-path (if (or (getf options :stdin) (getf options :stdin-data))
                                                     (first positionals)
                                                    (third positionals)))
                                       (outer fail))
                                  (lambda (code message &rest keys)
                                   (apply outer code message (append keys (list :path first-path))))))
                      (flet ((prepare (pattern replacement paths record-options)
                                      (let ((nth-text (getf options :nth)))
                                       (when (and nth-text (not (and (parse-count nth-text) (plusp (parse-count
                                                                                                    nth-text)))))
                                        (return-from prepare (funcall fail "argument.invalid" (format nil
                                                                                                      (concatenate
                                                                                                       'string
                                                                                                       "--nth ~S must "
                                                                                                       "be a positive "
                                                                                                       "integer")
                                                                                                      nth-text))))
                                       (compile-pattern/k
                                        pattern fail
                                        (lambda (regex)
                                         (%replacement-function/k
                                          replacement regex (getf options :literal-replacement)
                                          (lambda (code message &key rewrite)
                                           (funcall fail code message
                                                    :repairs (and rewrite
                                                                  (list (repair "use-template"
                                                                                "Use ${N} for a group reference."
                                                                                (command-line (options-argv
                                                                                               "replace" (list*
                                                                                                          pattern
                                                                                                          rewrite
                                                                                                          paths)
                                                                                               record-options)))))))
                                          (lambda (expand)
                                           (let ((replacer (make-replacer regex expand :nth (and nth-text
                                                                                                 (parse-count
                                                                                                  nth-text)))))
                                            (parse-selector/k
                                             :replace options :on-error fail
                                             :on-selector (lambda (selector)
                                                           (if (/= (length paths) 1)
                                                             (funcall fail "argument.invalid"
                                                                      "a selector needs exactly one file path")
                                                            (%replace-with-targets env paths options fail on-plan
                                                                                   replacer selector
                                                                                   pattern replacement
                                                                                   record-options)))
                                             :on-none (lambda ()
                                                       (%replace-with-targets env paths options fail on-plan
                                                                              replacer nil
                                                                              pattern replacement
                                                                              record-options)))))))
                                        :fixed
                                        (getf options :fixed) :word (getf options :word) :ignore-case (getf
                                                                                                       options
                                                                                                       :ignore-case)))))
                       (if (or (getf options :stdin) (getf options :stdin-data))
                         (read-stdin-json/k ports options
                                            :on-error fail
                                            :on-json (lambda (value text)
                                                      (let ((pattern (cdr (%json-member value "pattern")))
                                                            (replacement (cdr (%json-member value "replacement"))))
                                                       (if (and (stringp pattern) (stringp replacement))
                                                         (prepare pattern replacement positionals
                                                                  (inline-stdin-options options text))
                                                        (funcall fail "argument.invalid"
                                                                 (concatenate 'string
                                                                              "replace --stdin reads "
                                                                              "{\"pattern\": string, "
                                                                              "\"replacement\": string}"
                                                                              ))))))
                        (if (< (length positionals) 2)
                          (funcall fail "argument.invalid" "replace needs PATTERN and REPLACEMENT (or --stdin)")
                         (prepare (first positionals) (second positionals) (cddr positionals) options)))))
