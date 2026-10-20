(in-package #:aitools.edit.application)

(define-write-command "move-lines"
                      (ports env positionals options on-plan fail)
                      (let ((source (first positionals))
                            (destination
                             (or (getf options :to) (first positionals))))
                      (multiple-value-bind (kind argument)
                      (%parse-to-position
                       (or (getf options :to-position) "end"))
                      (cond
                      ((or (null source) (rest positionals))
                       (funcall fail
                                "argument.invalid"
                                "move-lines takes exactly one source path"))
                      ((null kind)
                       (funcall fail
                                "argument.invalid"
                                (format nil
                                        (concatenate 'string
                                                     "--to-position ~S must be "
                                                     "start, end, after:N, "
                                                     "before:N, after-symbol:NAME "
                                                     "or before-symbol:NAME")
                                        (getf options :to-position))))
                      (t
                       (parse-selector/k :move-lines
                                         options
                                         :on-error
                                         fail
                                         :on-none
                                         (lambda ()
                                         (funcall fail
                                                  "argument.invalid"
                                                  "move-lines needs a selector for the lines to move"))
                                         :on-selector
                                         (lambda (selector)
                                         (let ((positional
                                                (or
                                                 (eq
                                                  (aitools.kernel.domain:selector-basis
                                                   selector)
                                                  :position)
                                                 (member kind
                                                         '(:after :before
                                                                  :after-symbol
                                                                  :before-symbol))))
                                               (same (null (getf options :to))))
                                         (funcall on-plan
                                                  (make-write-plan :command
                                                                   "move-lines"
                                                                   :targets
                                                                   (list
                                                                    (make-write-target
                                                                     source)
                                                                    (make-write-target
                                                                     destination))
                                                                   :guard-requirements
                                                                   (append
                                                                    (and
                                                                     positional
                                                                     (if same
                                                                     (list
                                                                      (list
                                                                       :expect-hash
                                                                       source))
                                                                     (list
                                                                      (list
                                                                       :expect-hash
                                                                       source)
                                                                      (list
                                                                       :expect-hash
                                                                       destination))))
                                                                    (and
                                                                     (eq
                                                                      (aitools.kernel.domain:selector-kind
                                                                       selector)
                                                                      :match)
                                                                     (list
                                                                      (list
                                                                       :expect-count))))
                                                                   :expect-hashes
                                                                   (getf options
                                                                         :expect-hash)
                                                                   :expect-count
                                                                   (getf options
                                                                         :expect-count)
                                                                   :replayable
                                                                   (and
                                                                    (not
                                                                     positional)
                                                                    (null
                                                                     (getf
                                                                      options
                                                                      :expect-hash)))
                                                                   :plan
                                                                   (%move-lines-plan
                                                                    selector
                                                                    kind
                                                                    argument
                                                                    same)
                                                                   :record-options
                                                                   (lambda
                                                                   (paths)
                                                                   (let ((copy
                                                                          (copy-list
                                                                           options)))
                                                                   (if (getf
                                                                        options
                                                                        :to)
                                                                   (setf (getf
                                                                          copy
                                                                          :to) (second
                                                                                paths))
                                                                   (remf copy
                                                                         :to))
                                                                   copy))
                                                                   :record-positionals
                                                                   (lambda
                                                                   (paths)
                                                                   (list
                                                                    (first
                                                                     paths)))))))))))))

