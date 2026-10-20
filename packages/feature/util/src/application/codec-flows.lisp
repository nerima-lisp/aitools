(in-package #:aitools.util.application)

(aitools.kernel.domain:define-flow
    (util-encode-flow ports scheme request &key on-ok on-error)
  (cond
    ((not (aitools.util.domain:codec-scheme-p scheme))
     (aitools.kernel.domain:flow-call
         (%choice-error "scheme" scheme +util-codec-schemes+ "util encode")
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))
    (t
     (aitools.kernel.domain:flow-call
         (resolve-input ports request (format nil "util encode ~A" scheme))
       (:on-input (octets &key text)
         (declare (ignore text))
         (aitools.kernel.domain:flow-continue
          :on-ok
          (list
           (cons "output"
                 (aitools.util.domain:encode-octets scheme octets)))))
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))))

(aitools.kernel.domain:define-flow
    (util-decode-flow ports scheme request
     &key to root lock-timeout dry-run tx display-argv on-ok on-error)
  "Decode to `output` text when the result is valid UTF-8; otherwise return
`binary:true`, `bytes`, and the decoded bytes in `output_hex`. Text output
contains `output` and `bytes`, with no `binary` member.

With TO, the write pipeline returns `changes` and `op_id`; TX returns `tx`
and `tx_op`, while DRY-RUN returns `dry_run`. Each write result also
contains `bytes`; the pipeline adds `next_commands` when it truncates a diff.
ROOT and LOCK-TIMEOUT are global options; DRY-RUN and TX apply only with TO."
  (cond
    ((not (aitools.util.domain:codec-scheme-p scheme))
     (aitools.kernel.domain:flow-call
         (%choice-error "scheme" scheme +util-codec-schemes+ "util decode")
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))
    ((and (null to) (or dry-run tx))
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid"
      "--dry-run and --tx apply only to a write: add --to <path>"
      :repairs (list (%schema-repair "util decode"))))
    (t
     (aitools.kernel.domain:flow-call
         (resolve-input ports request (format nil "util decode ~A" scheme))
       (:on-input (octets &key text)
         (declare (ignore text))
         (aitools.kernel.domain:flow-call
             (aitools.util.domain:decode-text scheme octets)
           (:on-decoded (decoded)
             (if to
                 (aitools.kernel.domain:flow-call
                     (%write-decoded ports scheme octets decoded to
                                     :root root :lock-timeout lock-timeout
                                     :dry-run dry-run :tx tx :display-argv display-argv)
                   (:on-ok (fields)
                     (aitools.kernel.domain:flow-continue :on-ok fields))
                   (:on-error (code message &key candidates diagnostics conflicts repairs)
                     (aitools.kernel.domain:flow-continue
                      :on-error code message
                      :candidates candidates
                      :diagnostics diagnostics
                      :conflicts conflicts
                      :repairs repairs)))
                 (aitools.kernel.domain:flow-call
                     (aitools.util.domain:octets->utf-8 decoded)
                   (:on-text (output)
                     (aitools.kernel.domain:flow-continue
                      :on-ok
                      (list
                       (cons "bytes" (length decoded))
                       (cons "output" output))))
                   (:on-invalid (offset)
                     (declare (ignore offset))
                     (aitools.kernel.domain:flow-continue
                      :on-ok
                      (list
                       (cons "binary" t)
                       (cons "bytes" (length decoded))
                       (cons "output_hex"
                             (aitools.util.domain:octets->hex decoded))))))))
           (:on-invalid (offset reason)
             (aitools.kernel.domain:flow-continue
              :on-error "input.syntax-error"
              (format nil "invalid ~A input at byte ~D: ~A"
                      scheme offset reason)
              :repairs (list (%schema-repair "util decode"))))))
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))))

(aitools.kernel.domain:define-flow
    (util-redact-flow ports request &key on-ok on-error)
  (aitools.kernel.domain:flow-call
      (resolve-input ports request "util redact")
    (:on-input (octets &key text)
      (multiple-value-bind (redacted count)
          (aitools.protocol.domain:redact-secrets (%input-text octets text))
        (aitools.kernel.domain:flow-continue
         :on-ok
         (list (cons "text" redacted) (cons "redactions" count)))))
    (:on-error (code message &key repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message :repairs repairs))))

(aitools.kernel.domain:define-flow
    (util-tokens-flow ports request &key on-ok on-error)
  (aitools.kernel.domain:flow-call
      (resolve-input ports request "util tokens")
    (:on-input (octets &key text)
      (aitools.kernel.domain:flow-continue
       :on-ok
       (aitools.util.domain:text-statistics (%input-text octets text)
                                            (length octets))))
    (:on-error (code message &key repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message :repairs repairs))))
