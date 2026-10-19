(in-package #:aitools.inspect.domain)

(aitools.kernel.domain:define-flow
 (%resolve-symbol lines
                  selector
                  language
                  &key
                  on-selected
                  on-no-match
                  on-ambiguous
                  on-invalid)
 (if (or (null language) (null (language-definitions language)))
     (aitools.kernel.domain:flow-continue :on-invalid
                                          "input.unsupported-language"
                                          "--symbol needs a file in a language of the definition table")
     (let* ((definitions (find-definitions lines language))
            (name (selector-symbol-name selector))
            (kind (selector-symbol-kind selector))
            (hits
             (remove-if-not
              (lambda (definition)
                (and (string= (definition-name definition) name)
                     (or (null kind)
                         (string= (definition-kind definition) kind))))
              definitions)))
       (cond
         ((null hits)
          (aitools.kernel.domain:flow-continue :on-no-match
                                               (mapcar
                                                (lambda (definition)
                                                  (json-object "line"
                                                               (definition-line
                                                                definition)
                                                               "end_line"
                                                               (definition-end-line
                                                                definition)
                                                               "kind"
                                                               (definition-kind
                                                                definition)
                                                               "name"
                                                               (definition-name
                                                                definition)))
                                                (rank-similar name
                                                              definitions
                                                              :key
                                                              #'definition-name
                                                              :count
                                                              +selection-candidate-count+))))
         ((rest hits)
          (aitools.kernel.domain:flow-continue :on-ambiguous
                                               (mapcar
                                                (lambda (definition)
                                                  (json-object "line"
                                                               (definition-line
                                                                definition)
                                                               "end_line"
                                                               (definition-end-line
                                                                definition)
                                                               "kind"
                                                               (definition-kind
                                                                definition)
                                                               "name"
                                                               (definition-name
                                                                definition)))
                                                hits)))
         (t
          (aitools.kernel.domain:flow-continue :on-selected
                                               (list
                                                (cons
                                                 (definition-line (first hits))
                                                 (definition-end-line
                                                  (first hits))))))))))

(aitools.kernel.domain:define-flow
 (resolve-line-selector/k lines
                          selector
                          &key
                          language
                          on-selected
                          on-no-match
                          on-ambiguous
                          on-invalid)
 "Resolve SELECTOR (a kernel selector other than :OLD) against LINES. Call
exactly one continuation: ON-SELECTED with ranges, ON-NO-MATCH with
candidates, ON-AMBIGUOUS with candidates, or ON-INVALID with a code and
message. RANGES are 1-based inclusive (START . END) conses in file order.
LANGUAGE selects the definition table for :SYMBOL."
 (declare (type function on-selected on-no-match on-ambiguous on-invalid))
 (ecase (selector-kind selector)
   (:range
    (aitools.kernel.domain:flow-call
     (%resolve-range lines selector)
     (:on-selected (ranges)
                   (aitools.kernel.domain:flow-continue
                    :on-selected
                    ranges))
     (:on-no-match (candidates)
                   (aitools.kernel.domain:flow-continue
                    :on-no-match
                    candidates))))
   (:match
    (aitools.kernel.domain:flow-call
     (%resolve-match lines selector)
     (:on-selected (ranges)
                   (aitools.kernel.domain:flow-continue
                    :on-selected
                    ranges))
     (:on-no-match (candidates)
                   (aitools.kernel.domain:flow-continue
                    :on-no-match
                    candidates))
     (:on-invalid (code message)
                  (aitools.kernel.domain:flow-continue
                   :on-invalid
                   code
                   message))))
   (:between
    (aitools.kernel.domain:flow-call
     (%resolve-between lines selector)
     (:on-selected (ranges)
                   (aitools.kernel.domain:flow-continue
                    :on-selected
                    ranges))
     (:on-no-match (candidates)
                   (aitools.kernel.domain:flow-continue
                    :on-no-match
                    candidates))
     (:on-ambiguous (candidates)
                    (aitools.kernel.domain:flow-continue
                     :on-ambiguous
                     candidates))
     (:on-invalid (code message)
                  (aitools.kernel.domain:flow-continue
                   :on-invalid
                   code
                   message))))
   (:symbol
    (aitools.kernel.domain:flow-call
     (%resolve-symbol lines selector language)
     (:on-selected (ranges)
                   (aitools.kernel.domain:flow-continue
                    :on-selected
                    ranges))
     (:on-no-match (candidates)
                   (aitools.kernel.domain:flow-continue
                    :on-no-match
                    candidates))
     (:on-ambiguous (candidates)
                    (aitools.kernel.domain:flow-continue
                     :on-ambiguous
                     candidates))
     (:on-invalid (code message)
                  (aitools.kernel.domain:flow-continue
                   :on-invalid
                   code
                   message))))))
