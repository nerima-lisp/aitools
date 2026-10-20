(in-package #:aitools.edit.application)

(defun %strip-leading-bom (string)
  (if (and (plusp (length string)) (char= (char string 0) (code-char #xFEFF)))
      (values (subseq string 1) t)
      (values string nil)))

(defun %transcode-plan (context commit reject from to replace-unmappable)
  (let ((path (context-path context)))
    (read-file-octets/k
     context path reject
     (lambda (octets mode)
       (declare (ignore mode))
       (let ((from (or from (let ((guess (aitools.text.domain:guess-encoding octets)))
                              (if (eq guess :unknown)
                                  (return-from %transcode-plan
                                               (funcall reject "input.unsupported-format"
                                                        (format nil "cannot tell ~A's encoding; pass --from" path)))
                                guess)))))
         (aitools.text.domain:decode-octets/k
          octets from
          :on-invalid (lambda (offset)
                        (funcall reject "input.syntax-error"
                                 (format nil "~A is not valid ~A at byte ~D" path (aitools.text.domain:encoding-name
                                                                                   from) offset)
                                 :diagnostics (list (json-object "path" path "offset" offset))))
          :on-decoded
          (lambda (text replacements)
            (declare (ignore replacements))
            (multiple-value-bind (body bom) (%strip-leading-bom text)
              (let ((text (if (and bom (member to '(:utf-8 :utf-16le :utf-16be)))
                              (concatenate 'string (string (code-char #xFEFF)) body)
                            body)))
                (aitools.text.domain:encode-string/k
                 text to
                 :replace-unmappable replace-unmappable
                 :on-unmappable (lambda (index char)
                                  (funcall reject "argument.invalid"
                                           (format nil
                                                   (concatenate 'string
                                                                "~A cannot represent "
                                                                "U+~4,'0X (character ~D); "
                                                                "pass --replace-unmappable "
                                                                "to write ?"
                                                   )
                                                   (aitools.text.domain:encoding-name to) (char-code char) index)
                                           :diagnostics (list (json-object "index" index
                                                                           "codepoint" (format nil "U+~4,'0X"
                                                                                               (char-code char))))
                                           :repairs (list (repair "replace-unmappable"
                                                                  "Write ? for unrepresentable characters."
                                                                  (concatenate 'string (write-context-command-line
                                                                                        context)
                                                                               " --replace-unmappable")))))
                 :on-encoded (lambda (encoded replaced)
                               (let ((fields (list (cons "from" (aitools.text.domain:encoding-name from))
                                                   (cons "to" (aitools.text.domain:encoding-name to))
                                                   (cons "replaced" replaced))))
                                 (funcall commit (if (equalp encoded octets)
                                                     '()
                                                   (list (aitools.store.domain:write-file-request path encoded)))
                                          fields)))))))))))))

(define-write-command "transcode"
                      (ports env positionals options on-plan fail)
                      (let* ((path (first positionals))
                             (from-name (getf options :from))
                             (to-name (or (getf options :to) "utf-8"))
                             (from
                              (and from-name
                                   (aitools.text.domain:find-encoding from-name)))
                             (to (aitools.text.domain:find-encoding to-name))
                             (names
                              (mapcar #'aitools.text.domain:encoding-name
                                      aitools.text.domain:*supported-encodings*)))
                      (cond
                      ((or (null path) (rest positionals))
                       (funcall fail
                                "argument.invalid"
                                "transcode takes exactly one path"))
                      ((and from-name (null from))
                       (funcall fail
                                "input.unsupported-format"
                                (format nil
                                        "unsupported --from ~S; supported: ~{~A~^, ~}"
                                        from-name
                                        names)))
                      ((null to)
                       (funcall fail
                                "input.unsupported-format"
                                (format nil
                                        "unsupported --to ~S; supported: ~{~A~^, ~}"
                                        to-name
                                        names)))
                      (t
                       (funcall on-plan
                                (make-write-plan :command
                                                 "transcode"
                                                 :targets
                                                 (list (make-write-target path))
                                                 :expect-hashes
                                                 (getf options :expect-hash)
                                                 :plan
                                                 (lambda (context commit reject)
                                                 (%transcode-plan context
                                                                  commit
                                                                  reject
                                                                  from
                                                                  to
                                                                  (getf options
                                                                        :replace-unmappable)))
                                                 :record-options
                                                 options
                                                 :record-positionals
                                                 (lambda (paths)
                                                 (list (first paths)))))))))

