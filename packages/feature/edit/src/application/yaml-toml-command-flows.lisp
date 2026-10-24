;;;; YAML/TOML write command preparation using the shared JSON operations.
(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow (%format-set/k format command ports positionals options &key on-plan on-error)
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
                        :serializer (edit-ports-serialize-document ports)
                        :repair-pointer tokens
                        :record-options saved-options
                        :record-positionals
                        (lambda (paths)
                          (list* (first paths) saved-positionals))
                        :inputs inputs))))))
      (cond
        ((null path)
         (aitools.kernel.domain:flow-continue
           :on-error "argument.invalid"
           (format nil "~A set needs PATH" (command-display-name command))))
        ((or (getf options :stdin) (getf options :stdin-data))
         (if (rest positionals)
             (aitools.kernel.domain:flow-continue
               :on-error "argument.invalid"
               (format nil "~A set --stdin reads {\"pointer\",\"value\"}; pass only PATH"
                       (command-display-name command)))
             (aitools.kernel.domain:flow-call
               (read-stdin-json/k ports options)
               (:on-error (code message)
                 (aitools.kernel.domain:flow-continue :on-error code message))
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
                         (format nil "~A set --stdin reads {\"pointer\": string, \"value\": any}"
                                 (command-display-name command)))))))))
        ((/= (length positionals) 3)
         (aitools.kernel.domain:flow-continue
           :on-error "argument.invalid"
           (format nil "~A set takes PATH POINTER VALUE (or --stdin)"
                   (command-display-name command))))
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

(aitools.kernel.domain:define-flow (%format-delete/k format command ports positionals options &key on-plan on-error)
  (if (/= (length positionals) 2)
      (aitools.kernel.domain:flow-continue
        :on-error "argument.invalid"
        (format nil "~A delete takes PATH POINTER"
                (command-display-name command)))
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
                            :serializer (edit-ports-serialize-document ports)
                            :repair-pointer tokens
                            :record-positionals
                            (lambda (paths)
                              (list (first paths) (second positionals)))))))))

(aitools.kernel.domain:define-flow
  (%format-stdin-command format command ports positionals options transform-for
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
       (read-stdin-json/k ports options)
       (:on-error (code message)
         (aitools.kernel.domain:flow-continue :on-error code message))
       (:on-json (patch text)
         (aitools.kernel.domain:flow-continue
           :on-plan
           (%format-plan format command (first positionals)
                         (funcall transform-for patch)
                         options
                         :serializer (edit-ports-serialize-document ports)
                         :record-options (inline-stdin-options options text)
                         :record-positionals (lambda (paths) (list (first paths)))
                         :inputs (list text))))))))

(aitools.kernel.domain:define-flow (%format-fmt/k format command ports positionals options &key on-plan on-error)
  (if (/= (length positionals) 1)
      (aitools.kernel.domain:flow-continue
        :on-error "argument.invalid"
        (format nil "~A fmt takes exactly one PATH"
                (command-display-name command)))
      (aitools.kernel.domain:flow-continue
        :on-plan
        (%format-plan format command (first positionals) (function identity)
                      options
                      :serializer (edit-ports-serialize-document ports)))))

(aitools.kernel.domain:define-flow
  (%format-command/k format verb ports positionals options &key on-plan on-error)
  (let ((command (format nil "~A.~A" format verb)))
    (cond
      ((string= verb "set")
       (aitools.kernel.domain:flow-call
         (%format-set/k format command ports positionals options)
         (:on-plan (plan)
           (aitools.kernel.domain:flow-continue :on-plan plan))
         (:on-error (code message)
           (aitools.kernel.domain:flow-continue :on-error code message))))
      ((string= verb "delete")
       (aitools.kernel.domain:flow-call
         (%format-delete/k format command ports positionals options)
         (:on-plan (plan)
           (aitools.kernel.domain:flow-continue :on-plan plan))
         (:on-error (code message)
           (aitools.kernel.domain:flow-continue :on-error code message))))
      ((string= verb "merge")
       (aitools.kernel.domain:flow-call
         (%format-stdin-command format command ports positionals options
                                (lambda (patch)
                                  (lambda (document)
                                    (json-merge-patch document patch))))
         (:on-plan (plan)
           (aitools.kernel.domain:flow-continue :on-plan plan))
         (:on-error (code message)
           (aitools.kernel.domain:flow-continue :on-error code message))))
      ((string= verb "patch")
       (aitools.kernel.domain:flow-call
         (%format-stdin-command format command ports positionals options
                                (lambda (operations)
                                  (lambda (document)
                                    (json-apply-patch document operations))))
         (:on-plan (plan)
           (aitools.kernel.domain:flow-continue :on-plan plan))
         (:on-error (code message)
           (aitools.kernel.domain:flow-continue :on-error code message))))
      (t
       (aitools.kernel.domain:flow-call
         (%format-fmt/k format command ports positionals options)
         (:on-plan (plan)
           (aitools.kernel.domain:flow-continue :on-plan plan))
         (:on-error (code message)
           (aitools.kernel.domain:flow-continue :on-error code message)))))))

(defmacro define-format-write-commands (format-name)
  (let ((format (string-downcase format-name)))
    `(progn
       ,@(loop for verb in '("set" "delete" "merge" "patch" "fmt")
               collect
               (let ((command (format nil "~A.~A" format verb)))
                 `(define-write-command ,command
                      (ports env positionals options &key on-plan on-error)
                    (declare (ignore env))
                    (aitools.kernel.domain:flow-call
                      (%format-command/k ,format ,verb ports positionals options)
                      (:on-plan (plan)
                        (aitools.kernel.domain:flow-continue :on-plan plan))
                      (:on-error (code message)
                        (aitools.kernel.domain:flow-continue :on-error code message)))))))))
