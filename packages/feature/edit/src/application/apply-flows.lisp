;;;;
;;;; `apply`: a unified diff from --stdin, every hunk checked
;;;; against the current files before the store writes any.
(in-package #:aitools.edit.application)

(define-write-command "apply" (ports env positionals options &key on-plan on-error)
                      (cond
                       (positionals (flow-continue :on-error "argument.invalid"
                                                   "apply takes no positional arguments; the diff comes from --stdin"))
                       ((not (or (getf options :stdin) (getf options :stdin-data)))
                        (flow-continue :on-error "argument.invalid"
                                       "apply reads the unified diff from --stdin (stdin is never read implicitly)"))
                       (t
                        (let ((fuzz (parse-count (or (getf options :fuzz) "3")))
                              (strip (and (getf options :strip) (parse-count (getf options :strip))))
                              (reverse (getf options :reverse)))
                          (cond
                           ((null fuzz) (flow-continue :on-error "argument.invalid" (format nil
                                                                                            (concatenate
                                                                                             'string
                                                                                             "--fuzz ~S must be a "
                                                                                             "non-negative integer")
                                                                                            (getf options :fuzz))))
                           ((and (getf options :strip) (null strip))
                            (flow-continue :on-error "argument.invalid" (format nil
                                                                                (concatenate 'string
                                                                                  "--strip ~S must be a "
                                                                                  "non-negative "
                                                                                  "integer")
                                                                                (getf options :strip))))
                           (t
                            (aitools.kernel.domain:flow-call
                             (read-stdin-text/k ports options)
                             (:on-error (code message &key candidates diagnostics conflicts repairs path)
                                        (flow-continue :on-error code message
                                                       :candidates candidates :diagnostics diagnostics
                                                       :conflicts conflicts :repairs repairs :path path))
                             (:on-text (text)
                                       (multiple-value-bind (file-patches parse-error)
                                           (%parse-apply-diff text)
                                         (if parse-error
                                             (flow-continue :on-error "input.syntax-error"
                                                            (format nil "malformed diff: ~A" parse-error))
                                           (let ((patches
                                                  (%prepare-apply-patches file-patches strip reverse)))
                                             (if (null patches)
                                                 (flow-continue :on-error "input.syntax-error"
                                                                "--stdin holds no unified diff hunks")
                                               (flow-continue :on-plan (make-write-plan
                                                                        :command "apply"
                                                                        ;; Resolve diff paths from the workspace root.
                                                                        :targets
                                                                        (%apply-targets patches)
                                                                        :inputs (list text)
                                                                        :expect-hashes (getf options :expect-hash)
                                                                        :replayable (null (getf options :expect-hash))
                                                                        :plan (%apply-plan
                                                                               (mapcar
                                                                                (lambda (patch) (cons nil patch))
                                                                                patches)
                                                                               fuzz reverse)
                                                                        :record-options
                                                                        (inline-stdin-options options text)
                                                                        :record-positionals (constantly '())))))))))
                            ))))))

(defun %prepare-apply-patches (file-patches strip reverse) (loop for file-patch in file-patches
                                                        for old = (%strip-diff-path
                                                                   (aitools.kernel.domain:file-patch-old-path
                                                                    file-patch)
                                                                   strip)
                                                        for new = (%strip-diff-path
                                                                   (aitools.kernel.domain:file-patch-new-path
                                                                    file-patch)
                                                                   strip)
                                                        for (source target) = (if reverse
                                                                                  (list new old)
                                                                                (list old new))
                                                        when (aitools.kernel.domain:file-patch-hunks file-patch)
                                                        collect (%make-file-patch
                                                                 source target
                                                                 (aitools.kernel.domain:file-patch-hunks file-patch))))

(defun %parse-apply-diff (text)
  (handler-case
      (values (aitools.kernel.domain:parse-unified-diff text :strip 0) nil)
    (error (condition)
      (values nil condition))))

(defun %apply-targets (patches)
  (loop for patch in patches
        append
        (list
         (make-write-target
          (or (%file-patch-source patch)
              (%file-patch-target patch))
          :base :root)
         (make-write-target
          (or (%file-patch-target patch)
              (%file-patch-source patch))
          :base :root))))
