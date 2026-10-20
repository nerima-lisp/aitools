(in-package #:aitools.edit.application)

(define-write-command "transform" (ports env positionals options &key on-plan on-error)
  (let ((path (first positionals))
        (ops (getf options :op)))
    (let ((invalid-option nil))
      (flet ((integer-option (name key)
               (let ((text (getf options key)))
                 (cond
                   ((null text) nil)
                   ((parse-count text))
                   (t
                    (progn
                      (unless invalid-option
                        (setf invalid-option (format
                                               nil
                                               (concatenate 'string "--~A ~S must be a non-negative " "integer")
                                               name
                                               text)))
                      nil))))))
        (cond
          ((or (null path) (rest positionals))
           (flow-continue :on-error "argument.invalid" "transform takes exactly one path"))
          ((null ops) (flow-continue :on-error "argument.invalid" "transform needs at least one --op"))
          (t
           (let ((unknown
                  (find-if-not
                   (lambda (op)
                     (or (member op +line-transform-ops+ :test #'string=)
                         (member op +whole-file-transform-ops+ :test #'string=)))
                   ops))
                 (width (integer-option "width" :width))
                 (key (integer-option "key" :key))
                 (columns (or (integer-option "columns" :columns) 80))
                 (seed (integer-option "seed" :seed)))
             (cond
               (invalid-option (flow-continue :on-error "argument.invalid" invalid-option))
               (unknown
                (flow-continue :on-error
                               "argument.invalid"
                               (format
                                 nil
                                 "unknown --op ~S; ops: ~{~A~^ ~}"
                                 unknown
                                 (append +line-transform-ops+ +whole-file-transform-ops+))))
               ((and (member "shuffle" ops :test #'string=) (null seed))
                (flow-continue :on-error
                               "argument.invalid"
                               "shuffle needs --seed N so the same input gives the same output"))
               ((and key (zerop key)) (flow-continue :on-error "argument.invalid" "--key counts fields from 1"))
               ((and (zerop columns)) (flow-continue :on-error "argument.invalid" "--columns must be positive"))
               (t
                (let ((language
                       (and (intersection ops '("comment" "uncomment") :test #'string=)
                            (aitools.text.domain:language-for-path path))))
                  (if (and (intersection ops '("comment" "uncomment") :test #'string=)
                           (or (null language)
                               (not
                                (or (aitools.text.domain:language-line-comment language)
                                    (aitools.text.domain:language-block-comment language)))))
                      (flow-continue :on-error
                                     "input.unsupported-language"
                                     (format
                                       nil
                                       "no comment syntax is known for ~A"
                                       path))
                      (aitools.kernel.domain:flow-call
                        (compile-pattern/k (or (getf options :delimiter) "\\s+"))
                        (:on-error (code message)
                          (flow-continue :on-error code message))
                        (:on-regex (delimiter)
                          (aitools.kernel.domain:flow-call
                            (parse-selector/k :transform options)
                            (:on-error (code message &key repairs)
                              (flow-continue :on-error code message :repairs repairs))
                            (:on-selector (selector)
                              (if (intersection ops +whole-file-transform-ops+ :test #'string=)
                                  (flow-continue :on-error
                                                 "argument.invalid"
                                                 (concatenate 'string
                                                              "whole-file ops (eol-lf eol-crlf final-newline "
                                                              "no-final-newline strip-bom) take no selector"))
                                  (flow-continue :on-plan
                                                 (%transform-plan-for path
                                                                      options
                                                                      ops
                                                                      selector
                                                                      (list :width
                                                                            width
                                                                            :key
                                                                            key
                                                                            :delimiter
                                                                            delimiter
                                                                            :columns
                                                                            columns
                                                                            :seed
                                                                            seed
                                                                            :language
                                                                            language)))))
                            (:on-none ()
                              (flow-continue :on-plan
                                             (%transform-plan-for path
                                                                  options
                                                                  ops
                                                                  nil
                                                                  (list :width
                                                                        width
                                                                        :key
                                                                        key
                                                                        :delimiter
                                                                        delimiter
                                                                        :columns
                                                                        columns
                                                                        :seed
                                                                        seed
                                                                        :language
                                                                        language))))))))))))))))))
