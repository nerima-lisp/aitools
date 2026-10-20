(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
 (%json-stdin-command name
                      ports
                      positionals
                      options
                      transform-for
                      &key
                      on-plan
                      on-error)
 "json merge / json patch: PATH plus a JSON document on --stdin, applied
by (TRANSFORM-FOR input) -> (value -> value)."
 (cond
   ((/= (length positionals) 1)
    (aitools.kernel.domain:flow-continue :on-error
                                         "argument.invalid"
                                         (format nil
                                                 "~A takes exactly one PATH"
                                                 (command-display-name name))))
   ((not (or (getf options :stdin) (getf options :stdin-data)))
    (aitools.kernel.domain:flow-continue :on-error
                                         "argument.invalid"
                                         (format nil
                                                 "~A reads the patch from --stdin (stdin is never read implicitly)"
                                                 (command-display-name name))))
   (t
        (aitools.kernel.domain:flow-call
      (read-stdin-json/k ports options)
      (:on-error (code message)
        (aitools.kernel.domain:flow-continue
          :on-error
          code
          message))
      (:on-json (input text)
        (aitools.kernel.domain:flow-continue
          :on-plan
          (make-write-plan :command
                           name
                           :targets
                           (list (make-write-target (first positionals)))
                           :inputs
                           (list text)
                           :expect-hashes
                           (getf options :expect-hash)
                           :replayable
                           (null (getf options :expect-hash))
                           :plan
                           (%json-plan (funcall transform-for input))
                           :record-options
                           (inline-stdin-options options text)
                           :record-positionals
                           (lambda (paths)
                             (list (first paths))))))))))

(define-write-command "json.merge"
                      (ports env positionals options &key on-plan on-error)
                      (aitools.kernel.domain:flow-call
                       (%json-stdin-command "json.merge"
                                            ports
                                            positionals
                                            options
                                            (lambda (patch)
                                              (lambda (document)
                                                (json-merge-patch document
                                                                  patch))))
                       (:on-plan (plan) (flow-continue :on-plan plan))
                       (:on-error (code message)
                                  (flow-continue :on-error code message))))

(define-write-command "json.patch"
                      (ports env positionals options &key on-plan on-error)
                      (aitools.kernel.domain:flow-call
                       (%json-stdin-command "json.patch"
                                            ports
                                            positionals
                                            options
                                            (lambda (operations)
                                              (lambda (document)
                                                (json-apply-patch document
                                                                  operations))))
                       (:on-plan (plan) (flow-continue :on-plan plan))
                       (:on-error (code message)
                                  (flow-continue :on-error code message))))

(define-write-command "json.fmt"
                      (ports env positionals options &key on-plan on-error)
                      (let ((indent (getf options :indent)))
                        (cond
                          ((/= (length positionals) 1)
                           (flow-continue :on-error
                                          "argument.invalid"
                                          "json fmt takes exactly one PATH"))
                          ((and indent
                                (not
                                 (and (parse-count indent)
                                      (<= (parse-count indent) 16))))
                           (flow-continue :on-error
                                          "argument.invalid"
                                          (format nil
                                                  "--indent ~S must be 0 to 16"
                                                  indent)))
                          ((and indent (getf options :minify))
                           (flow-continue :on-error
                                          "argument.invalid"
                                          "--indent and --minify cannot be combined"))
                          (t
                           (flow-continue :on-plan
                                          (make-write-plan :command
                                                           "json.fmt"
                                                           :targets
                                                           (list
                                                            (make-write-target
                                                             (first positionals)))
                                                           :expect-hashes
                                                           (getf options
                                                                 :expect-hash)
                                                           :replayable
                                                           (null
                                                            (getf options
                                                                  :expect-hash))
                                                           :plan
                                                           (%json-plan
                                                            #'identity
                                                            :sort-keys
                                                            (getf options
                                                                  :sort-keys)
                                                            :indent-function
                                                            (lambda (detected)
                                                              (cond
                                                                ((getf options
                                                                       :minify)
                                                                 nil)
                                                                (indent
                                                                 (make-string
                                                                  (parse-count
                                                                   indent)
                                                                  :initial-element
                                                                  #\Space))
                                                                (t
                                                                 (or detected
                                                                     "  ")))))
                                                           :record-options
                                                           options
                                                           :record-positionals
                                                           (lambda (paths)
                                                             (list
                                                              (first paths)))))))))
