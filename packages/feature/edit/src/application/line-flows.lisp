;;;; packages/feature/edit/src/application/line-flows.lisp
;;;;
;;;; `transform` and `move-lines`.
(in-package #:aitools.edit.application)

(defun %runs (ranges)
  "RANGES (1-based inclusive, ascending) merged into contiguous runs."
  (let ((runs '()))
    (dolist (range ranges (nreverse runs))
      (if (and runs (= (car range) (1+ (cdr (first runs)))))
          (setf (cdr (first runs)) (cdr range))
          (push (cons (car range) (cdr range)) runs)))))

(defun %transform-selection (document ranges function)
  "DOCUMENT with FUNCTION (a list of lines -> list of lines) applied to the
selected lines. A contiguous selection is replaced as one block; a
scattered one (--match) gets its lines back in place when FUNCTION keeps
their number, else FUNCTION runs on each contiguous run."
  (let ((runs (%runs ranges)))
    (flet ((lines-of (run)
             (loop for index from (1- (car run)) below (cdr run)
                   collect (document-line document index))))
      (if (null (rest runs))
          (let ((run (first runs)))
            (document-replace-lines document (1- (car run)) (cdr run) (funcall function (lines-of run))))
          (let* ((selected (mapcan #'lines-of runs))
                 (result (funcall function selected)))
            (if (= (length result) (length selected))
                (let ((lines (copy-seq (text-document-lines document))))
                  (dolist (run runs)
                    (loop for index from (1- (car run)) below (cdr run)
                          do (setf (svref lines index) (pop result))))
                  (document-with-lines document lines))
                (let ((result document))
                  (dolist (run (reverse runs) result)
                    (setf result (document-replace-lines result
                                                         (1- (car run))
                                                         (cdr run)
                                                         (funcall function (lines-of run))))))))))))

(defun %whole-file-op (document op)
  (cond
    ((string= op "eol-lf") (document-with-eol document +lf+))
    ((string= op "eol-crlf") (document-with-eol document +crlf+))
    ((string= op "final-newline") (document-with-final-newline document t))
    ((string= op "no-final-newline") (document-with-final-newline document nil))
    ((string= op "strip-bom") (document-without-bom document))))

(defun %transform-line-ops (ops lines keywords)
  (dolist (op ops lines)
    (setf lines (apply #'transform-lines op lines keywords))))

(defun %transform-all-ops (document ops keywords)
  (let ((result document))
    (dolist (op ops result)
      (setf result (cond
                     ((member op +whole-file-transform-ops+ :test #'string=) (%whole-file-op result op))
                     ((plusp (document-line-count result))
                      (%transform-selection result
                                            (list (cons 1 (document-line-count result)))
                                            (lambda (lines)
                                              (apply #'transform-lines op lines keywords))))
                     (t result))))))

(defun %removed-line-extra (ops original result)
  (when
      (some
       (lambda (op)
         (member op '("unique" "delete-blank" "squeeze-blank") :test #'string=))
       ops)
    (list (cons "removed_lines" (max 0 (- original (document-line-count result)))))))

(aitools.kernel.domain:define-flow
  (%commit-transform-ops/k context path document ops ranges keywords &key on-committed)
  (let* ((original (document-line-count document))
         (result
          (if ranges
              (%transform-selection document
                                    ranges
                                    (lambda (lines)
                                      (%transform-line-ops ops lines keywords)))
              (%transform-all-ops document ops keywords))))
    (aitools.kernel.domain:flow-call
      (commit-document context path result :extra (%removed-line-extra ops original result))
      (:on-committed (requests &optional extra)
        (aitools.kernel.domain:flow-continue
          :on-committed
          requests
          extra)))))

(aitools.kernel.domain:define-flow
  (%transform-plan-from-document/k context path document ops selector keywords &key on-committed on-rejected)
  (if selector
      (aitools.kernel.domain:flow-call
        (resolve-lines/k document selector path)
        (:on-error (code message &key (candidates nil candidates-p))
          (if candidates-p
              (aitools.kernel.domain:flow-continue
                :on-rejected
                code
                message
                :candidates
                candidates)
              (aitools.kernel.domain:flow-continue
                :on-rejected
                code
                message)))
        (:on-ranges (ranges)
          (if (eq (aitools.kernel.domain:selector-kind selector) :match)
              (aitools.kernel.domain:flow-call
                (check-expect-count/k context (length ranges))
                (:on-rejected (code message &key diagnostics)
                  (aitools.kernel.domain:flow-continue
                    :on-rejected
                    code
                    message
                    :diagnostics
                    diagnostics))
                (:on-ok ()
                  (aitools.kernel.domain:flow-call
                    (%commit-transform-ops/k context path document ops ranges keywords)
                    (:on-committed (requests &optional extra)
                      (aitools.kernel.domain:flow-continue
                        :on-committed
                        requests
                        extra)))))
              (aitools.kernel.domain:flow-call
                (%commit-transform-ops/k context path document ops ranges keywords)
                (:on-committed (requests &optional extra)
                  (aitools.kernel.domain:flow-continue
                    :on-committed
                    requests
                    extra))))))
      (aitools.kernel.domain:flow-call
        (%commit-transform-ops/k context path document ops nil keywords)
        (:on-committed (requests &optional extra)
          (aitools.kernel.domain:flow-continue
            :on-committed
            requests
            extra)))))

(aitools.kernel.domain:define-flow
  (%run-transform-plan/k context path ops selector keywords &key on-committed on-rejected)
  (aitools.kernel.domain:flow-call
    (read-document/k context path)
    (:on-error (code message &key candidates diagnostics conflicts repairs path)
      (declare (ignore path))
      (aitools.kernel.domain:flow-continue
        :on-rejected
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
    (:on-document (document)
      (aitools.kernel.domain:flow-call
        (%transform-plan-from-document/k context path document ops selector keywords)
        (:on-committed (requests &optional extra)
          (aitools.kernel.domain:flow-continue
            :on-committed
            requests
            extra))
        (:on-rejected (code message &key (candidates nil candidates-p)
                                          (diagnostics nil diagnostics-p))
          (cond
            ((and candidates-p diagnostics-p)
             (aitools.kernel.domain:flow-continue
               :on-rejected
               code
               message
               :candidates candidates
               :diagnostics diagnostics))
            (candidates-p
             (aitools.kernel.domain:flow-continue
               :on-rejected
               code
               message
               :candidates candidates))
            (diagnostics-p
             (aitools.kernel.domain:flow-continue
               :on-rejected
               code
               message
               :diagnostics diagnostics))
            (t
             (aitools.kernel.domain:flow-continue :on-rejected code message))))))
    (:on-filtered ())))

(defun %transform-plan (ops selector keywords)
  (lambda (context &key on-committed on-rejected)
    (%run-transform-plan/k context
                           (context-path context)
                           ops
                           selector
                           keywords
                           :on-committed
                           on-committed
                           :on-rejected
                           on-rejected)))

(defun %transform-plan-for (path options ops selector keywords)
  (make-write-plan :command
                   "transform"
                   :targets
                   (list (make-write-target path))
                   :guard-requirements
                   (selector-guards selector path)
                   :expect-hashes
                   (getf options :expect-hash)
                   :expect-count
                   (getf options :expect-count)
                   :replayable
                   (and (content-selector-p selector) (null (getf options :expect-hash)))
                   :plan
                   (%transform-plan ops selector keywords)
                   :record-options
                   options
                   :record-positionals
                   (lambda (paths)
                     (list (first paths)))))
