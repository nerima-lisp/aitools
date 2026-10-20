(in-package #:aitools.edit.application)

(define-write-command "replace"
                      (ports env positionals options &key on-plan on-error)
                      ;; A refusal without its own :PATH is about the first file searched, not the
                      ;; first positional (the pattern), so its default repair names a file.
                      (flet
                            ((prepare (pattern replacement paths record-options)
                                      (let ((nth-text (getf options :nth))) (if (and nth-text
                                                   (not (and (parse-count nth-text)
                                                             (plusp (parse-count nth-text)))))
                                                                           (flow-continue :on-error
                                                                             "argument.invalid"
                                                                             (format nil
                                                                               "--nth ~S must be a positive integer"
                                                                               nth-text))
 (aitools.kernel.domain:flow-call
  (compile-pattern/k pattern
                     :fixed
                     (getf options :fixed)
                     :word
                     (getf options :word)
                     :ignore-case
                     (getf options :ignore-case))
  (:on-error (code message)
    (flow-continue :on-error code message))
  (:on-regex (regex)
    (aitools.kernel.domain:flow-call
      (%replacement-function/k replacement regex (getf options :literal-replacement))
      (:on-error (code message &key rewrite)
  (flow-continue
    :on-error
    code
    message
    :repairs
    (and rewrite
         (list
          (repair
            "use-template"
            "Use ${N} for a group reference."
            (command-line
              (options-argv
                "replace"
                (list* pattern rewrite paths)
                record-options)))))))
      (:on-function (expand)
        (let ((replacer
               (make-replacer regex expand :nth (and nth-text (parse-count nth-text)))))
          (aitools.kernel.domain:flow-call
            (parse-selector/k :replace options)
            (:on-selector (selector)
              (if (> (length paths) 1)
                  (flow-continue :on-error "argument.invalid"
                           "a selector needs exactly one file path")
                  (aitools.kernel.domain:flow-call
                    (%replace-with-targets env
                                           paths
                                           options
                                           replacer
                                           selector
                                           pattern
                                           replacement
                                           record-options)
                    (:on-error (code message
                               &key candidates diagnostics conflicts repairs path)
                      (flow-continue :on-error code message
                                     :candidates candidates
                                     :diagnostics diagnostics
                                     :conflicts conflicts
                                     :repairs repairs
                                     :path path))
                    (:on-plan (plan)
                      (flow-continue :on-plan plan)))))
            (:on-none ()
              (aitools.kernel.domain:flow-call
                (%replace-with-targets env
                                       paths
                                       options
                                       replacer
                                       nil
                                       pattern
                                       replacement
                                       record-options)
                (:on-error (code message
                           &key candidates diagnostics conflicts repairs path)
                  (flow-continue :on-error code message
                                 :candidates candidates
                                 :diagnostics diagnostics
                                 :conflicts conflicts
                                 :repairs repairs
                                 :path path))
                (:on-plan (plan)
                  (flow-continue :on-plan plan))))
            (:on-error (code message &key repairs)
              (flow-continue :on-error code message :repairs repairs))))))))))))
                          (if (or (getf options :stdin)
                                  (getf options :stdin-data))
                              (aitools.kernel.domain:flow-call
                               (read-stdin-json/k ports options)
                               (:on-error (code message)
                                          (flow-continue :on-error code message))
                               (:on-json (value text)
                                         (let ((pattern (cdr (%json-member value "pattern")))
                                               (replacement (cdr (%json-member value "replacement"))))
                                           (if (and (stringp pattern) (stringp replacement))
                                               (prepare pattern
                                                        replacement
                                                        positionals
                                                        (inline-stdin-options options text))
                                             (flow-continue :on-error "argument.invalid"
                                                            (concatenate (quote string)
                                                                         "replace --stdin reads {\"pattern\": string, "
                                                                         "\"replacement\": string}"))))))

                            (if (< (length positionals) 2)
                                (flow-continue :on-error "argument.invalid"
                                               "replace needs PATTERN and REPLACEMENT (or --stdin)")
                              (prepare
                               (first positionals)
                               (second positionals)
                               (cddr positionals)
                               options)))))
