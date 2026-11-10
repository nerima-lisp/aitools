(in-package #:aitools.md.application)

(defun %md-section-number (value)
  (let ((number
         (cond
           ((integerp value) value)
           ((and (stringp value)
                 (plusp (length value))
                 (every
                  (lambda (character)
                    (char<= #\0 character #\9))
                  value)) (parse-integer value)))))
    (and number (plusp number) number)))

(defun %md-heading-json (heading)
  (json-object "section"
               (aitools.md.domain:md-heading-section heading)
               "heading"
               (aitools.md.domain:md-heading-heading heading)
               "level"
               (aitools.md.domain:md-heading-level heading)
               "line"
               (aitools.md.domain:md-heading-line heading)))

(aitools.kernel.domain:define-flow (md-headings/k boundaries path &key root on-ok on-partial on-error)
                                   (declare (type function on-ok on-partial on-error))
                                   (aitools.kernel.domain:flow-call (%md-read-document/k boundaries path root "md headings")
                                                                    (:on-document (document)
                                                                                  (aitools.kernel.domain:flow-continue
                                                                                   :on-ok
                                                                                   (list (cons "path" path)
                                                                                         (cons "hash"
                                                                                               (aitools.kernel.domain:content-hash
                                                                                                (aitools.md.domain:md-document-octets
                                                                                                 document)))
                                                                                         (cons "headings"
                                                                                               (mapcar
                                                                                                #'%md-heading-json
                                                                                                (aitools.md.domain:md-document-headings
                                                                                                 document))))))
                                                                    (:on-error (code message &key diagnostics repairs)
                                                                               (aitools.kernel.domain:flow-continue
                                                                                :on-error
                                                                                code
                                                                                message
                                                                                :diagnostics
                                                                                diagnostics
                                                                                :repairs
                                                                                repairs))))

(aitools.kernel.domain:define-flow (md-read/k boundaries path section &key root on-ok on-partial on-error)
                                   (declare (type function on-ok on-partial on-error))
                                   (let ((number (%md-section-number section)))
                                     (if (null number)
                                         (aitools.kernel.domain:flow-continue :on-error
                                                                              "argument.invalid"
                                                                              "section must be a positive heading ordinal"
                                                                              :repairs
                                                                              (%md-schema-repair "md read"
                                                                                                 "Pass a positive section ordinal."))
                                         (aitools.kernel.domain:flow-call
                                          (%md-read-document/k boundaries path root "md read")
                                          (:on-document (document)
                                                        (let ((heading
                                                               (aitools.md.domain:md-document-heading document number)))
                                                          (if (null heading)
                                                              (aitools.kernel.domain:flow-continue :on-error
                                                                                                   "selection.no-match"
                                                                                                   (format nil
                                                                                                           "section ~D does not exist in ~A"
                                                                                                           number
                                                                                                           path)
                                                                                                   :repairs
                                                                                                   (%md-schema-repair
                                                                                                    "md read"
                                                                                                    "Choose an ordinal from md headings."))
                                                              (aitools.kernel.domain:flow-continue :on-ok
                                                                                                   (list
                                                                                                    (cons "path" path)
                                                                                                    (cons "hash"
                                                                                                          (aitools.kernel.domain:content-hash
                                                                                                           (aitools.md.domain:md-document-octets
                                                                                                            document)))
                                                                                                    (cons "section"
                                                                                                          number)
                                                                                                    (cons "heading"
                                                                                                          (aitools.md.domain:md-heading-heading
                                                                                                           heading))
                                                                                                    (cons "level"
                                                                                                          (aitools.md.domain:md-heading-level
                                                                                                           heading))
                                                                                                    (cons "line"
                                                                                                          (aitools.md.domain:md-heading-line
                                                                                                           heading))
                                                                                                    (cons "content"
                                                                                                          (subseq
                                                                                                           (aitools.md.domain:md-document-source
                                                                                                            document)
                                                                                                           (aitools.md.domain:md-heading-start-char
                                                                                                            heading)
                                                                                                           (aitools.md.domain:md-heading-end-char
                                                                                                            heading))))))))
                                          (:on-error (code message &key diagnostics repairs)
                                                     (aitools.kernel.domain:flow-continue :on-error
                                                                                          code
                                                                                          message
                                                                                          :diagnostics
                                                                                          diagnostics
                                                                                          :repairs
                                                                                          repairs))))))
