(in-package #:aitools.edit.application)

(defun %strip-diff-path (path strip)
  (cond
    ((or (null path) (string= path "/dev/null")) nil)
    (strip (aitools.kernel.domain:strip-path-components path strip))
    ((and (> (length path) 2) (member (subseq path 0 2) '("a/" "b/") :test #'string=)) (subseq path 2))
    (t path)))

(defun %strip-cr-hunk (hunk)
  (aitools.kernel.domain:make-diff-hunk
   (aitools.kernel.domain:diff-hunk-old-start hunk) (aitools.kernel.domain:diff-hunk-old-count hunk)
   (aitools.kernel.domain:diff-hunk-new-start hunk) (aitools.kernel.domain:diff-hunk-new-count hunk)
   (mapcar (lambda (line)
             (let ((text (aitools.kernel.domain:diff-line-text line)))
               (aitools.kernel.domain:make-diff-line
                (aitools.kernel.domain:diff-line-op line)
                (string-right-trim '(#\Return) text)
                :no-newline (aitools.kernel.domain:diff-line-no-newline line))))
           (aitools.kernel.domain:diff-hunk-lines hunk))))

(defun %patched-final-newline (hunks reverse default)
  "Whether the patched file ends with a newline: the new side's last line
marker when the last hunk reaches the end of the file, else DEFAULT."
  (let* ((lines (aitools.kernel.domain:diff-hunk-lines (car (last hunks))))
         (new-op (if reverse :delete :insert))
         (old-op (if reverse :insert :delete))
         (new-side (remove-if (lambda (line) (eq (aitools.kernel.domain:diff-line-op line) old-op)) lines))
         (old-side (remove-if (lambda (line) (eq (aitools.kernel.domain:diff-line-op line) new-op)) lines)))
    (cond ((and new-side (aitools.kernel.domain:diff-line-no-newline (car (last new-side)))) nil)
          ((and old-side (aitools.kernel.domain:diff-line-no-newline (car (last old-side)))) t)
          (t default))))

(defstruct (%file-patch (:constructor %make-file-patch (source target hunks)) (:copier nil))
  ;; SOURCE: the path whose content the hunks apply to (NIL: created);
  ;; TARGET: where the result goes (NIL: deleted)
  source target hunks)

(defun %apply-plan (patches fuzz reverse)
(lambda (context commit reject)
(block plan
(let ((requests '())
      (applied 0)
      (paths (write-context-paths context))
      (index -1))
(progn
(dolist (patch (mapcar #'cdr patches))
(incf index)
(let* ((source (%file-patch-source patch))
       (target (%file-patch-target patch))
       (source-path (and source (nth (* 2 index) paths)))
       (target-path (and target (nth (1+ (* 2 index)) paths)))
       (hunks (mapcar #'%strip-cr-hunk (%file-patch-hunks patch))))
(flet ((finish (lines final-newline count eol bom)
       (incf applied count)
       (if (null target-path)
       (progn
       (when
       (some
        (lambda (line)
        (plusp (length line)))
        lines)
       (return-from plan
       (funcall reject
                "selection.no-match"
                (format nil
                        "the patch deletes ~A but content remains"
                        source-path))))
       (push (aitools.store.domain:delete-request source-path) requests))
       (let* ((document (make-text-document "" :bom-p bom :eol eol))
              (document (document-replace-lines document 0 0 lines)))
       (push
        (write-document-request target-path
                                (document-with-final-newline document
                                                             final-newline))
        requests)
       (when (and source-path (string/= source-path target-path))
       (push (aitools.store.domain:delete-request source-path) requests))))))
(if (null source-path)
(let ((state
       (aitools.store.application:view-path-state (write-context-view context)
                                                  target-path)))
(unless (aitools.store.domain:entry-state-absent-p state)
(return-from plan
(funcall reject
         "refusal.exists"
         (format nil "the patch creates ~A, which exists" target-path))))
(aitools.kernel.domain:apply-hunks/k '()
                                     hunks
                                     :fuzz
                                     fuzz
                                     :reverse
                                     reverse
                                     :on-applied
                                     (lambda (lines final count)
                                     (declare (ignore final))
                                     (finish lines
                                             (%patched-final-newline hunks
                                                                     reverse
                                                                     t)
                                             count
                                             +lf+
                                             nil))
                                     :on-conflict
                                     (lambda (hunk-index hunk nearby)
                                     (declare (ignore hunk nearby))
                                     (return-from plan
                                     (funcall reject
                                              "selection.no-match"
                                              (format nil
                                                      "hunk ~D does not apply to new file ~A"
                                                      (1+ hunk-index)
                                                      target-path))))))
(read-document/k context
                 source-path
                 (lambda (&rest rejection)
                 (return-from plan
                 (apply reject rejection)))
                 (lambda (document)
                 (aitools.kernel.domain:apply-hunks/k
                  (coerce (text-document-lines document) 'list)
                  hunks
                  :fuzz
                  fuzz
                  :reverse
                  reverse
                  :on-applied
                  (lambda (lines final count)
                  (declare (ignore final))
                  (finish lines
                          (%patched-final-newline hunks
                                                  reverse
                                                  (document-final-newline-p
                                                   document))
                          count
                          (text-document-eol document)
                          (text-document-bom-p document)))
                  :on-conflict
                  (lambda (hunk-index hunk nearby)
                  (return-from plan
                  (funcall reject
                           "selection.no-match"
                           (format nil
                                   "hunk ~D of ~A does not apply within --fuzz ~D"
                                   (1+ hunk-index)
                                   source-path
                                   fuzz)
                           :candidates
                           (list
                            (json-object "path"
                                         source-path
                                         "hunk"
                                         (1+ hunk-index)
                                         "expected_start"
                                         (aitools.kernel.domain:diff-hunk-old-start
                                          hunk)
                                         "expected"
                                         (mapcar
                                          #'aitools.kernel.domain:diff-line-text
                                          (remove
                                           (if reverse
                                           :delete
                                           :insert)
                                           (aitools.kernel.domain:diff-hunk-lines
                                            hunk)
                                           :key
                                           #'aitools.kernel.domain:diff-line-op))
                                         "current"
                                         nearby)))))))))))))
(funcall commit (nreverse requests) (list (cons "applied_hunks" applied)))))))

