(in-package #:aitools.edit.application)

(defun %replace-plan (replacer multiline selector explicit skipped)
  "EXPLICIT: paths the user named as files, whose unreadable or non-text
state is an error; scanned files in that state are skipped."
  (lambda (context &key on-committed on-rejected)
    (let ((requests '())
          (per-change '())
          (total 0)
          (failure nil)
          ;; A byte-level required-literal check, so a file lacking
          ;; the pattern's literal is skipped without decoding. Sound only
          ;; where a miss means zero replacements and no error, so it is
          ;; applied only to scanned (non-explicit) files and dropped when a
          ;; selector restricts the lines. A prefilter match still goes
          ;; through the replacement matcher.
          (literal (and (null selector) (replacer-required-literal replacer))))
      (labels ((reject-or-skip (path code message
                 &key candidates diagnostics conflicts repairs)
                 (when (member path explicit :test #'equal)
                   (setf failure
                     (list code message
                       :candidates candidates
                       :diagnostics diagnostics
                       :conflicts conflicts
                       :repairs repairs))))
               (replace-in (path document ranges)
                 (reset-replacer replacer)
                 (multiple-value-bind (result count)
                     (replace-document document replacer ranges multiline)
                   (when (plusp count)
                     (incf total count)
                     (push (write-document-request path result) requests)
                     (push (cons path (list (cons "count" count))) per-change))))
               (process-path (path)
                 (aitools.kernel.domain:flow-call
                   (read-document/k context
                                     path
                                     :prefilter
                                     (and literal
                                          (not (member path explicit :test #'equal))
                                          (lambda (octets)
                                            (octets-search literal octets))))
                   (:on-error
                     (code message
                           &key candidates diagnostics conflicts repairs
                             ((:path ignored-path)))
                     (declare (ignore ignored-path))
                     (reject-or-skip path
                                     code
                                     message
                                     :candidates candidates
                                     :diagnostics diagnostics
                                     :conflicts conflicts
                                     :repairs repairs))
                   (:on-filtered ())
                   (:on-document (document)
                     (if selector
                         (aitools.kernel.domain:flow-call
                           (resolve-lines/k document selector path)
                           (:on-error
                             (code message
                                   &key candidates diagnostics conflicts repairs path)
                             (declare (ignore path))
                             (reject-or-skip path
                                             code
                                             message
                                             :candidates candidates
                                             :diagnostics diagnostics
                                             :conflicts conflicts
                                             :repairs repairs))
                           (:on-ranges (ranges)
                             (replace-in path document ranges)))
                         (replace-in path document nil))))
))
        (dolist (path (write-context-paths context))
          (unless failure
            (process-path path)))
        (if failure
            (destructuring-bind (code message
                                 &key candidates diagnostics conflicts repairs)
                failure
              (funcall on-rejected
                       code
                       message
                       :candidates candidates
                       :diagnostics diagnostics
                       :conflicts conflicts
                       :repairs repairs))
            (aitools.kernel.domain:flow-call
              (check-expect-count/k context total)
              (:on-rejected
                (code message
                      &key candidates diagnostics conflicts repairs path)
                (declare (ignore path))
                (funcall on-rejected
                         code
                         message
                         :candidates candidates
                         :diagnostics diagnostics
                         :conflicts conflicts
                         :repairs repairs))
              (:on-ok ()
                (funcall on-committed
                         (nreverse requests)
                         (append
                           (list (cons :per-change per-change))
                           (when skipped
                             (list
                               (cons "skipped"
                                     (nreverse skipped)))))))))))))
