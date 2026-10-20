(in-package #:aitools.edit.application)

(defun %strip-leading-bom (string)
  (if (and (plusp (length string)) (char= (char string 0) (code-char #xFEFF)))
      (values (subseq string 1) t)
      (values string nil)))

(aitools.kernel.domain:define-flow
  (%transcode-plan context from to replace-unmappable &key on-error on-committed)
  (let ((path (context-path context)))
    (labels ((reject-invalid (encoding offset)
               (flow-continue
                 :on-error
                 "input.syntax-error"
                 (format nil
                         "~A is not valid ~A at byte ~D"
                         path
                         (aitools.text.domain:encoding-name encoding)
                         offset)
                 :diagnostics
                 (list (json-object "path" path "offset" offset))))
             (reject-unmappable (index char)
               (flow-continue
                 :on-error
                 "argument.invalid"
                 (format nil
                         (concatenate
                           'string
                           "~A cannot represent U+~4,'0X "
                           "(character ~D); pass "
                           "--replace-unmappable to write ?")
                         (aitools.text.domain:encoding-name to)
                         (char-code char)
                         index)
                 :diagnostics
                 (list (json-object
                         "index" index
                         "codepoint"
                         (format nil "U+~4,'0X" (char-code char))))
                 :repairs
                 (list (repair
                         "replace-unmappable"
                         "Write ? for unrepresentable characters."
                         (concatenate
                           'string
                           (write-context-command-line context)
                           " --replace-unmappable")))))
             (commit-encoded (octets encoding encoded replaced)
               (flow-continue
                 :on-committed
                 (if (equalp encoded octets)
                     '()
                     (list (aitools.store.domain:write-file-request path encoded)))
                 (list
                   (cons "from" (aitools.text.domain:encoding-name encoding))
                   (cons "to" (aitools.text.domain:encoding-name to))
                   (cons "replaced" replaced))))
             (encode-text (text octets encoding)
               (aitools.text.domain:encode-string/k
                 text
                 to
                 :replace-unmappable
                 replace-unmappable
                 :on-unmappable
                 #'reject-unmappable
                 :on-encoded
                 (lambda (encoded replaced)
                   (commit-encoded octets encoding encoded replaced))))
             (decode-text (octets encoding)
               (aitools.text.domain:decode-octets/k
                 octets
                 encoding
                 :on-invalid
                 (lambda (offset)
                   (reject-invalid encoding offset))
                 :on-decoded
                 (lambda (text replacements)
                   (declare (ignore replacements))
                   (multiple-value-bind (body bom) (%strip-leading-bom text)
                     (let ((decoded-text
                             (if (and bom
                                      (member to
                                              '(:utf-8
                                                :utf-16le
                                                :utf-16be)))
                                 (concatenate 'string
                                              (string (code-char #xFEFF))
                                              body)
                                 body)))
                       (encode-text decoded-text octets encoding)))))))
      (aitools.kernel.domain:flow-call
        (read-file-octets/k context path)
        (:on-error (code message &key candidates diagnostics conflicts repairs path)
          (flow-continue
            :on-error
            code
            message
            :candidates
            candidates
            :diagnostics
            diagnostics
            :conflicts
            conflicts
            :repairs
            repairs
            :path
            path))
        (:on-octets (octets mode)
          (declare (ignore mode))
          (let* ((guess (and (null from) (aitools.text.domain:guess-encoding octets)))
                 (encoding (or from (and (not (eq guess :unknown)) guess))))
            (if (null encoding)
                (flow-continue
                  :on-error
                  "input.unsupported-format"
                  (format nil "cannot tell ~A's encoding; pass --from" path))
                (decode-text octets encoding))))))))

(define-write-command "transcode"
                      (ports env positionals options &key on-plan on-error)
                      (let* ((path (first positionals))
                             (from-name (getf options :from))
                             (to-name (or (getf options :to) "utf-8"))
                             (from (and from-name (aitools.text.domain:find-encoding from-name)))
                             (to (aitools.text.domain:find-encoding to-name))
                             (names
                              (mapcar #'aitools.text.domain:encoding-name aitools.text.domain:*supported-encodings*)))
                        (cond
                          ((or (null path) (rest positionals))
                           (flow-continue :on-error "argument.invalid" "transcode takes exactly one path"))
                          ((and from-name (null from))
                           (flow-continue :on-error
                                          "input.unsupported-format"
                                          (format nil "unsupported --from ~S; supported: ~{~A~^, ~}" from-name names)))
                          ((null to)
                           (flow-continue :on-error
                                          "input.unsupported-format"
                                          (format nil "unsupported --to ~S; supported: ~{~A~^, ~}" to-name names)))
                          (t
                           (flow-continue :on-plan
                                          (make-write-plan :command
                                                           "transcode"
                                                           :targets
                                                           (list (make-write-target path))
                                                           :expect-hashes
                                                           (getf options :expect-hash)
                                                           :plan
                                                           (lambda (context &key on-committed on-rejected)
                                                             (aitools.kernel.domain:flow-call
                                                               (%transcode-plan context
                                                                                from
                                                                                to
                                                                                (getf options :replace-unmappable))
                                                               (:on-error
                                                                (code message
                                                                      &key
                                                                      candidates
                                                                      diagnostics
                                                                      conflicts
                                                                      repairs
                                                                      path)
                                                                (declare (ignore path))
                                                                (funcall on-rejected
                                                                         code
                                                                         message
                                                                         :candidates
                                                                         candidates
                                                                         :diagnostics
                                                                         diagnostics
                                                                         :conflicts
                                                                         conflicts
                                                                         :repairs
                                                                         repairs))
                                                               (:on-committed (requests fields)
                                                                              (funcall on-committed requests fields))))
                                                           :record-options
                                                           options
                                                           :record-positionals
                                                           (lambda (paths)
                                                             (list (first paths)))))))))
