;;;;
;;;; `apply`: a unified diff from --stdin, every hunk checked
;;;; against the current files before the store writes any.
(in-package #:aitools.edit.application)

(defun %strip-diff-path (path strip)
  (cond
    ((or (null path) (string= path "/dev/null")) nil)
    (strip (aitools.kernel.domain:strip-path-components path strip))
    ((and (> (length path) 2) (member (subseq path 0 2) '("a/" "b/") :test #'string=)) (subseq path 2))
    (t path)))

(defun %strip-cr-hunk (hunk)
  (aitools.kernel.domain:make-diff-hunk (aitools.kernel.domain:diff-hunk-old-start hunk)
                                        (aitools.kernel.domain:diff-hunk-old-count hunk)
                                        (aitools.kernel.domain:diff-hunk-new-start hunk)
                                        (aitools.kernel.domain:diff-hunk-new-count hunk)
                                        (mapcar
                                         (lambda (line)
                                           (let ((text (aitools.kernel.domain:diff-line-text line)))
                                             (aitools.kernel.domain:make-diff-line
                                              (aitools.kernel.domain:diff-line-op line)
                                              (string-right-trim '(#\Return) text)
                                              :no-newline
                                              (aitools.kernel.domain:diff-line-no-newline line))))
                                         (aitools.kernel.domain:diff-hunk-lines hunk))))

(defun %patched-final-newline (hunks reverse default)
  "Whether the patched file ends with a newline: the new side's last line
marker when the last hunk reaches the end of the file, else DEFAULT."
  (let* ((lines (aitools.kernel.domain:diff-hunk-lines (car (last hunks))))
         (new-op
          (if reverse
              :delete
              :insert))
         (old-op
          (if reverse
              :insert
              :delete))
         (new-side
          (remove-if
           (lambda (line)
             (eq (aitools.kernel.domain:diff-line-op line) old-op))
           lines))
         (old-side
          (remove-if
           (lambda (line)
             (eq (aitools.kernel.domain:diff-line-op line) new-op))
           lines)))
    (cond
      ((and new-side (aitools.kernel.domain:diff-line-no-newline (car (last new-side)))) nil)
      ((and old-side (aitools.kernel.domain:diff-line-no-newline (car (last old-side)))) t)
      (t default))))

(defstruct (%file-patch (:constructor %make-file-patch (source target hunks)) (:copier nil))
  ;; SOURCE: the path whose content the hunks apply to (NIL: created);
  ;; TARGET: where the result goes (NIL: deleted)
  source target hunks)

(defun %apply-plan (patches fuzz reverse)
  (lambda (context &key on-committed on-rejected)
    (let ((requests '())
          (applied 0)
          (paths (write-context-paths context))
          (index -1)
          (failure nil))
      (labels ((reject (&rest arguments)
                 (unless failure
                   (setf failure arguments)))
               (finish (source-path target-path lines final-newline count eol bom)
                 (incf applied count)
                 (if (null target-path)
                     (if (some
                          (lambda (line)
                            (plusp (length line)))
                          lines)
                         (reject "selection.no-match"
                                 (format
                                   nil
                                   "the patch deletes ~A but content remains"
                                   source-path))
                         (push (aitools.store.domain:delete-request source-path) requests))
                     (let* ((document (make-text-document "" :bom-p bom :eol eol))
                            (document (document-replace-lines document 0 0 lines)))
                       (push (write-document-request target-path (document-with-final-newline document final-newline))
                             requests)
                       (when (and source-path (string/= source-path target-path))
                         (push (aitools.store.domain:delete-request source-path) requests)))))
               (apply-patch (source-path target-path hunks)
                 (if (null source-path)
                     (let ((state (aitools.store.application:view-path-state (write-context-view context) target-path)))
                       (if (aitools.store.domain:entry-state-absent-p state)
                           (aitools.kernel.domain:flow-call
                            (aitools.kernel.domain:apply-hunks/k '() hunks :fuzz fuzz :reverse reverse)
                            (:on-applied (lines final count)
                                         (declare (ignore final))
                                         (finish source-path
                                                 target-path
                                                 lines
                                                 (%patched-final-newline hunks reverse t)
                                                 count
                                                 +lf+
                                                 nil))
                            (:on-conflict (hunk-index hunk nearby)
                                          (declare (ignore hunk nearby))
                                          (reject "selection.no-match"
                                                  (format nil "hunk ~D does not apply to new file ~A"
                                                          (1+ hunk-index)
                                                          target-path))))
                           (reject "refusal.exists"
                                   (format
                                     nil
                                     "the patch creates ~A, which exists"
                                     target-path))))
                     (aitools.kernel.domain:flow-call
                       (read-document/k context source-path)
                       (:on-error (code message &key candidates diagnostics conflicts repairs ((:path ignored-path)))
                         (declare (ignore ignored-path))
                         (setf failure (list code
                                             message
                                             :candidates
                                             candidates
                                             :diagnostics
                                             diagnostics
                                             :conflicts
                                             conflicts
                                             :repairs
                                             repairs)))
                       (:on-document (document)
                                     (aitools.kernel.domain:flow-call
                                      (aitools.kernel.domain:apply-hunks/k
                                       (coerce (text-document-lines document) 'list)
                                       hunks
                                       :fuzz fuzz
                                       :reverse reverse)
                                      (:on-applied (lines final count)
                                                   (declare (ignore final))
                                                   (let ((document-newline (document-final-newline-p document)))
                                                     (finish source-path
                                                             target-path
                                                             lines
                                                             (%patched-final-newline hunks reverse document-newline)
                                                             count
                                                             (text-document-eol document)
                                                             (text-document-bom-p document))))
                                      (:on-conflict (hunk-index hunk nearby)
                                                    (flet ((conflict-record ()
                                                                            (json-object
                                                                             "path" source-path
                                                                             "hunk" (1+ hunk-index)
                                                                             "expected_start"
                                                                             (aitools.kernel.domain:diff-hunk-old-start hunk)
                                                                             "expected"
                                                                             (mapcar #'aitools.kernel.domain:diff-line-text
                                                                                     (remove (if reverse :delete :insert)
                                                                                             (aitools.kernel.domain:diff-hunk-lines hunk)
                                                                                             :key #'aitools.kernel.domain:diff-line-op))
                                                                             "current" nearby)))
                                                      (reject "selection.no-match"
                                                              (format nil
                                                                      (concatenate 'string
                                                                                   "hunk ~D of ~A does not "
                                                                                   "apply within --fuzz ~D")
                                                                      (1+ hunk-index)
                                                                      source-path
                                                                      fuzz)
                                                              :candidates (list (conflict-record)))))))
                       (:on-filtered ())))))
        (dolist (patch (mapcar #'cdr patches))
          (unless failure
            (incf index)
            (let* ((source (%file-patch-source patch))
                   (target (%file-patch-target patch))
                   (source-path (and source (nth (* 2 index) paths)))
                   (target-path (and target (nth (1+ (* 2 index)) paths)))
                   (hunks (mapcar #'%strip-cr-hunk (%file-patch-hunks patch))))
              (apply-patch source-path target-path hunks))))
        (if failure
            (destructuring-bind (code message &key candidates diagnostics conflicts repairs) failure
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
          (funcall on-committed (nreverse requests) (list (cons "applied_hunks" applied))))))))
