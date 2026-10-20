(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (read-flow ports
            path
            &key
            root
            tx
            lock-timeout
            range
            symbol
            kind
            between
            exclusive
            match
            invert
            tail
            (as "text")
            bytes
            (min-length 4)
            escape-invisible
            encoding
            (max-lines 80)
            on-ok
            on-partial
            on-error)
 "`read`. Calls exactly one of ON-OK, ON-PARTIAL (fields) or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (let ((mode
        (cond
          ((string= as "hex") :hex)
          ((string= as "strings") :strings)
          (t :text)))
       (charset (and encoding (find-encoding encoding))))
   (labels ((invalid (message)
              (aitools.kernel.domain:flow-continue :on-error
                                                   "argument.invalid"
                                                   message
                                                   :repairs
                                                   (list
                                                    (repair "read-file"
                                                            "Read the file with the defaults."
                                                            (format nil
                                                                    "aitools read ~A"
                                                                    (shell-quote
                                                                     path))))))
            (continue-with (selector)
              (aitools.kernel.domain:flow-call
               (%read-with-selector ports
                                    path
                                    selector
                                    mode
                                    :root
                                    root
                                    :tx
                                    tx
                                    :lock-timeout
                                    lock-timeout
                                    :tail
                                    tail
                                    :bytes
                                    bytes
                                    :min-length
                                    min-length
                                    :escape-invisible
                                    escape-invisible
                                    :charset
                                    charset
                                    :max-lines
                                    max-lines)
               (:on-ok (fields)
                       (aitools.kernel.domain:flow-continue :on-ok fields))
               (:on-partial (fields)
                            (aitools.kernel.domain:flow-continue :on-partial
                                                                 fields))
               (:on-error (code message &key candidates diagnostics conflicts repairs)
                          (aitools.kernel.domain:flow-continue
                           :on-error code message
                           :candidates candidates
                           :diagnostics diagnostics
                           :conflicts conflicts
                           :repairs repairs)))))
     (cond
       ((and encoding (null charset))
        (aitools.kernel.domain:flow-continue :on-error
                                             "input.unsupported-format"
                                             (format nil
                                                     "unknown encoding ~S; supported: ~{~A~^, ~}"
                                                     encoding
                                                     (mapcar #'encoding-name
                                                             *supported-encodings*))
                                             :repairs
                                             (list
                                              (repair "guess-encoding"
                                                      "Ask info for the encoding guess."
                                                      (format nil
                                                              "aitools info ~A"
                                                              (shell-quote path))))))
       ((and bytes (not (eq mode :hex))) (invalid "--bytes needs --as hex"))
       ((and (member mode '(:hex :strings)) (or encoding escape-invisible))
        (invalid "--encoding and --escape-invisible apply to --as text only"))
       (t
        (aitools.kernel.domain:flow-call
         (parse-selector-options/k :read
                                   :range
                                   range
                                   :symbol
                                   symbol
                                   :kind
                                   kind
                                   :between
                                   between
                                   :exclusive
                                   exclusive
                                   :match
                                   match
                                   :invert
                                   invert
                                   :extra-exclusive
                                   (list (cons "--tail" tail)
                                         (cons "--as hex" (eq mode :hex))
                                         (cons "--as strings"
                                               (eq mode :strings))))
         (:on-invalid (message repairs)
                      (aitools.kernel.domain:flow-continue :on-error
                                                           "argument.invalid"
                                                           message
                                                           :repairs
                                                           repairs))
         (:on-none ()
                      (continue-with nil))
         (:on-selector (selector)
                       (continue-with selector))))))))
