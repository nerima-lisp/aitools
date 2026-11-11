;;;; Document write command preparation using the shared JSON operations.
(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow (%format-set/k format command boundaries positionals options &key on-plan on-error)
  (let ((path (first positionals)))
    (labels ((plan (pointer value saved-options saved-positionals inputs)
               (multiple-value-bind (tokens refusal)
                   (handler-case (values (parse-json-pointer pointer) nil)
                     (edit-refusal (condition) (values nil condition)))
                 (if refusal
                     (aitools.kernel.domain:flow-continue
                       :on-error (edit-refusal-code refusal)
                       (edit-refusal-detail refusal))
                     (aitools.kernel.domain:flow-continue
                       :on-plan
                       (%format-plan
                        format command path
                        (lambda (document)
                          (json-add document tokens value :array-mode :replace))
                        options
                        :serializer (and boundaries (cl-boundary-kit:boundary-context-require boundaries :edit-serialize-document))
                        :repair-pointer tokens
                        :record-options saved-options
                        :record-positionals
                        (lambda (paths)
                          (list* (first paths) saved-positionals))
                        :inputs inputs))))))
      (cond
        ((or (getf options :stdin) (getf options :stdin-data))
         (if (rest positionals)
             (aitools.kernel.domain:flow-continue
               :on-error "argument.invalid"
               (format nil "~A --stdin reads {\"pointer\",\"value\"}; pass only PATH" (command-display-name command)))
             (aitools.kernel.domain:flow-call
               (read-stdin-json/k boundaries options)
               (:on-error (code message
                   &key candidates diagnostics conflicts repairs path)
                 (aitools.kernel.domain:flow-continue
                   :on-error code message
                   :candidates candidates
                   :diagnostics diagnostics
                   :conflicts conflicts
                   :repairs repairs
                   :path path))
               (:on-json (input text)
                 (let ((pointer-member (%json-member input "pointer"))
                       (value-member (%json-member input "value")))
                   (if (and pointer-member value-member
                            (stringp (cdr pointer-member)))
                       (plan (cdr pointer-member)
                             (cdr value-member)
                             (inline-stdin-options options text)
                             '()
                             (list text))
                       (aitools.kernel.domain:flow-continue
                         :on-error "argument.invalid"
                         (format nil "~A --stdin reads {\"pointer\": string, \"value\": any}" (command-display-name command)))))))))
        ((/= (length positionals) 3)
         (aitools.kernel.domain:flow-continue
           :on-error "argument.invalid"
           (format nil "~A takes PATH POINTER VALUE (or --stdin)" (command-display-name command))))
        (t
         (let ((text (third positionals)))
           (aitools.kernel.domain:flow-call
             (parse-json-text/k text)
             (:on-value (value)
               (plan (second positionals) value options (rest positionals)
                     (list text)))
             (:on-invalid (message)
               (declare (ignore message))
               (aitools.kernel.domain:flow-continue
                 :on-error "argument.invalid"
                 (format nil "VALUE ~S is not JSON; a string needs JSON quotes"
                         text))))))))))

(aitools.kernel.domain:define-flow (%format-delete/k format command boundaries positionals options &key on-plan on-error)
  (if (/= (length positionals) 2)
      (aitools.kernel.domain:flow-continue
        :on-error "argument.invalid"
        (format nil "~A takes PATH POINTER" (command-display-name command)))
      (multiple-value-bind (tokens refusal)
          (handler-case (values (parse-json-pointer (second positionals)) nil)
            (edit-refusal (condition) (values nil condition)))
        (if refusal
            (aitools.kernel.domain:flow-continue
              :on-error (edit-refusal-code refusal) (edit-refusal-detail refusal))
            (aitools.kernel.domain:flow-continue
              :on-plan
              (%format-plan format command (first positionals)
                            (lambda (document) (json-remove document tokens))
                            options
                            :serializer (and boundaries (cl-boundary-kit:boundary-context-require boundaries :edit-serialize-document))
                            :repair-pointer tokens
                            :record-positionals
                            (lambda (paths)
                              (list (first paths) (second positionals)))))))))

(aitools.kernel.domain:define-flow
  (%format-stdin-command format command boundaries positionals options transform-for
                         &key on-plan on-error)
  "Read a JSON merge patch or JSON Patch document from --stdin."
  (cond
    ((/= (length positionals) 1)
     (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid"
       (format nil "~A takes exactly one PATH" (command-display-name command))))
    ((not (or (getf options :stdin) (getf options :stdin-data)))
     (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid"
       (format nil "~A reads the patch from --stdin (stdin is never read implicitly)"
               (command-display-name command))))
    (t
     (aitools.kernel.domain:flow-call
       (read-stdin-json/k boundaries options)
       (:on-error (code message
                   &key candidates diagnostics conflicts repairs path)
                 (aitools.kernel.domain:flow-continue
                   :on-error code message
                   :candidates candidates
                   :diagnostics diagnostics
                   :conflicts conflicts
                   :repairs repairs
                   :path path))
       (:on-json (patch text)
         (aitools.kernel.domain:flow-continue
           :on-plan
           (%format-plan format command (first positionals)
                         (funcall transform-for patch)
                         options
                         :serializer (and boundaries (cl-boundary-kit:boundary-context-require boundaries :edit-serialize-document))
                         :record-options (inline-stdin-options options text)
                         :record-positionals (lambda (paths) (list (first paths)))
                         :inputs (list text))))))))

(defmacro %with-plan-continuations (flow)
  `(aitools.kernel.domain:flow-call
    ,flow
    (:on-plan (plan)
              (aitools.kernel.domain:flow-continue :on-plan plan))
    (:on-error (code message
                     &key candidates diagnostics conflicts repairs path)
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :candidates candidates
                :diagnostics diagnostics
                :conflicts conflicts
                :repairs repairs
                :path path))))

(aitools.kernel.domain:define-flow
 (%format-command/k format verb boundaries positionals options &key on-plan on-error)
 (let ((command (format nil "doc.~A" verb)))
   (cond
    ((string= verb "set")
     (%with-plan-continuations
      (%format-set/k format command boundaries positionals options)))
    ((string= verb "delete")
     (%with-plan-continuations
      (%format-delete/k format command boundaries positionals options)))
    ((string= verb "merge")
     (%with-plan-continuations
      (%format-stdin-command
       format command boundaries positionals options
       (lambda (patch)
         (lambda (document)
           (json-merge-patch document patch))))))
    ((string= verb "patch")
     (%with-plan-continuations
      (%format-stdin-command
       format command boundaries positionals options
       (lambda (operations)
         (lambda (document)
           (json-apply-patch document operations))))))
    (t
     (%with-plan-continuations
      (%format-fmt/k format command boundaries positionals options))))))

(aitools.kernel.domain:define-flow
 (%prepare-doc-command/k verb boundaries positionals options &key on-plan on-error)
 (let ((path (first positionals)))
   (cond
    ((null path)
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid"
      (format nil "doc ~A needs PATH" verb)))
    (t
     (let ((format (document-format path (getf options :format))))
       (if format
           (%with-plan-continuations
            (%format-command/k format verb boundaries positionals options))
         (aitools.kernel.domain:flow-continue
          :on-error "argument.invalid"
          (format nil
                  "Cannot determine the document format for ~A; pass --format json, yaml, or toml."
                  path)
          :repairs (list (%document-format-repair verb positionals options)))))))))

(defmacro define-doc-write-commands ()
  `(progn
     ,@(loop for verb in (list "set" "delete" "merge" "patch" "fmt")
             collect
             `(define-write-command ,(format nil "doc.~A" verb)
                                    (boundaries env positionals options &key on-plan on-error)
                                    (declare (ignore env))
                                    (aitools.kernel.domain:flow-call
                                     (%prepare-doc-command/k ,verb boundaries positionals options)
                                     (:on-plan (plan)
                                               (aitools.kernel.domain:flow-continue :on-plan plan))
                                     (:on-error (code message
                                                      &key candidates diagnostics conflicts repairs path)
                                                (aitools.kernel.domain:flow-continue
                                                 :on-error code message
                                                 :candidates candidates
                                                 :diagnostics diagnostics
                                                 :conflicts conflicts
                                                 :repairs repairs
                                                 :path path)))))))
(define-doc-write-commands)
