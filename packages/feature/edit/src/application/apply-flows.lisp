;;;; packages/feature/edit/src/application/apply-flows.lisp
;;;;
;;;; `apply`: a unified diff from --stdin, every hunk checked
;;;; against the current files before the store writes any.
(in-package #:aitools.edit.application)

(define-write-command "apply" (ports env positionals options on-plan fail)
  (cond
    (positionals (funcall fail "argument.invalid" "apply takes no positional arguments; the diff comes from --stdin"))
    ((not (or (getf options :stdin) (getf options :stdin-data)))
     (funcall fail "argument.invalid" "apply reads the unified diff from --stdin (stdin is never read implicitly)"))
    (t
     (let ((fuzz (parse-count (or (getf options :fuzz) "3")))
           (strip (and (getf options :strip) (parse-count (getf options :strip))))
           (reverse (getf options :reverse)))
       (cond
         ((null fuzz) (funcall fail "argument.invalid" (format nil "--fuzz ~S must be a non-negative integer" (getf options :fuzz))))
         ((and (getf options :strip) (null strip))
          (funcall fail "argument.invalid" (format nil "--strip ~S must be a non-negative integer" (getf options :strip))))
         (t
          (read-stdin-text/k
           ports options :on-error fail
           :on-text (lambda (text)
                     (block %apply-on-text
                      (let* ((file-patches (handler-case (aitools.kernel.domain:parse-unified-diff text :strip 0)
                                             (error (condition)
                                               (return-from %apply-on-text
                                                 (funcall fail "input.syntax-error" (format nil "malformed diff: ~A" condition))))))
                             (patches (loop for file-patch in file-patches
                                            for old = (%strip-diff-path (aitools.kernel.domain:file-patch-old-path file-patch) strip)
                                            for new = (%strip-diff-path (aitools.kernel.domain:file-patch-new-path file-patch) strip)
                                            for (source target) = (if reverse (list new old) (list old new))
                                            when (aitools.kernel.domain:file-patch-hunks file-patch)
                                              collect (%make-file-patch source target (aitools.kernel.domain:file-patch-hunks file-patch)))))
                        (if (null patches)
                            (funcall fail "input.syntax-error" "--stdin holds no unified diff hunks")
                            (funcall on-plan
                                     (make-write-plan
                                      :command "apply"
                                      ;; A diff names files relative to the workspace root, as
                                      ;; `git diff` prints them, wherever apply runs.
                                      :targets (loop for patch in patches
                                                     append (list (make-write-target (or (%file-patch-source patch) (%file-patch-target patch))
                                                                                     :base :root)
                                                                  (make-write-target (or (%file-patch-target patch) (%file-patch-source patch))
                                                                                     :base :root)))
                                      :inputs (list text)
                                      :expect-hashes (getf options :expect-hash)
                                      :replayable (null (getf options :expect-hash))
                                      :plan (%apply-plan (mapcar (lambda (patch) (cons nil patch)) patches) fuzz reverse)
                                      :record-options (inline-stdin-options options text)
                                      :record-positionals (constantly '()))))))))))))))
