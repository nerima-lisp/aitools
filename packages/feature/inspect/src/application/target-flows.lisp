(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (call-with-target-text/k context target
                           &key on-text on-binary on-missing on-unreadable)
 "The binary-checked read of the :FILE target TARGET for text use: ON-TEXT (octets),
ON-BINARY (prefix size) when the first 8 KiB hold a NUL, ON-MISSING () for a
genuinely absent file, or ON-UNREADABLE () for a file that exists but was
denied (EACCES). The tx view holds the bytes in memory; on disk the source
opens the file once and reads nothing past the sniffed prefix of a binary file."
 (declare (type function on-text on-binary on-missing on-unreadable))
 (if (%through-view-p context target)
     (let ((octets (view-read-file (inspect-context-view context)
                                   (file-target-relative target))))
       (cond
         ((null octets)
          (aitools.kernel.domain:flow-continue :on-missing))
         ((binary-octets-p octets)
          (aitools.kernel.domain:flow-continue
           :on-binary
           (subseq octets 0 (min (length octets) +binary-sniff-length+))
           (length octets)))
         (t (aitools.kernel.domain:flow-continue :on-text octets))))
     (aitools.text.application:call-with-sniffed-octets/k
      (context-source context)
      (file-target-real target)
      :on-text (lambda (octets)
                 (aitools.kernel.domain:flow-continue :on-text octets))
      :on-binary (lambda (prefix size)
                   (aitools.kernel.domain:flow-continue :on-binary prefix size))
      :on-missing (lambda (path)
                    (declare (ignore path))
                    (aitools.kernel.domain:flow-continue :on-missing))
      :on-unreadable (lambda (path)
                       (declare (ignore path))
                       (aitools.kernel.domain:flow-continue :on-unreadable)))))
(aitools.kernel.domain:define-flow
 (call-with-readable-file/k context path &key on-file on-error)
 "Probe PATH and call ON-FILE (target) for a regular file; a missing path
or a directory ends in ON-ERROR with its repairs."
 (declare (type function on-file on-error))
 (let ((target (probe-target context path)))
   (case (file-target-kind target)
     (:file
      (aitools.kernel.domain:flow-continue :on-file target))
     (:absent
      (aitools.kernel.domain:flow-call
       (%read-target-error/k context target nil)
       (:on-error (code message &key candidates diagnostics conflicts repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message
          :candidates candidates
          :diagnostics diagnostics
          :conflicts conflicts
          :repairs repairs))))
     (t
      (aitools.kernel.domain:flow-call
       (fail-not-a-file context target)
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs)))))))
(aitools.kernel.domain:define-flow
 (record-read/k context target &key on-recorded on-error)
 "With `--tx`, record TARGET's disk state in the tx's read set (a
path outside the workspace cannot be in it), then call ON-RECORDED ()."
 (declare (type function on-recorded on-error))
 (if (and (inspect-context-tx context)
          (file-target-relative target)
          (plusp (length (file-target-relative target))))
     (tx-record-read/k
      (inspect-context-store context)
      (inspect-context-tx context)
      (file-target-relative target)
      :lock-timeout-ms (inspect-context-lock-timeout-ms context)
      :on-recorded (lambda (state)
                     (declare (ignore state))
                     (aitools.kernel.domain:flow-continue :on-recorded))
      :on-not-found (lambda ()
                      (aitools.kernel.domain:flow-continue
                       :on-error
                       "input.not-found"
                       (format nil "tx ~A does not exist" (inspect-context-tx context))
                       :repairs
                       (list (repair "list-tx" "List the open transactions."
                                     "aitools tx status"))))
      :on-busy
      (lambda ()
        (aitools.kernel.domain:flow-continue
         :on-error
         "environment.busy"
         "the tx lock could not be acquired within --lock-timeout"
         :repairs
         (list
          (repair "retry" "Retry once the tx is idle."
                  (command-line context
                                (list "read" (file-target-argument target)))))))
     )
     (aitools.kernel.domain:flow-continue :on-recorded)))
(aitools.kernel.domain:define-flow
 (call-with-inspect-file/k ports path
                           &key root tx lock-timeout record on-file on-error)
 "Resolve the inspect context, probe PATH as a readable regular file, and,
when RECORD (and --tx), record the read before calling ON-FILE (context
target). The context/readable-file/record chain every single-file flow opens."
 (declare (type function on-file on-error))
 (aitools.kernel.domain:flow-call
  (inspect-context-flow ports :root root :tx tx :lock-timeout lock-timeout)
  (:on-ready (context)
    (aitools.kernel.domain:flow-call
     (call-with-readable-file/k context path)
     (:on-file (target)
       (if record
           (aitools.kernel.domain:flow-call
            (record-read/k context target)
            (:on-recorded ()
              (aitools.kernel.domain:flow-continue :on-file context target))
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message :repairs repairs)))
           (aitools.kernel.domain:flow-continue :on-file context target)))
     (:on-error
      (code message &key candidates diagnostics conflicts repairs)
       (aitools.kernel.domain:flow-continue
        :on-error code message
        :candidates candidates
        :diagnostics diagnostics
        :conflicts conflicts
        :repairs repairs))))
  (:on-error (code message &key repairs)
    (aitools.kernel.domain:flow-continue
     :on-error code message :repairs repairs))))