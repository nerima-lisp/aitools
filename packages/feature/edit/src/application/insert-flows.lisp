(in-package #:aitools.edit.application)

(defun %insert-plan (text mode selector)
  "MODE is :START, :END, :BEFORE or :AFTER."
  (lambda (context &key on-committed on-rejected)
    (let ((path (context-path context))
          (lines (content-lines text)))
      (aitools.kernel.domain:flow-call
        (read-document/k context path)
        (:on-error (code message &key candidates diagnostics conflicts repairs ((:path ignored-path)))
          (declare (ignore ignored-path))
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
        (:on-document (document)
          (flet ((done (result inserted-at)
                   (aitools.kernel.domain:flow-call
                     (commit-document context path result :extra (list (cons "inserted_at" inserted-at)))
                     (:on-committed (requests &optional extra)
                       (funcall on-committed requests extra)))))
            (ecase mode
              (:start (done (document-replace-lines document 0 0 lines) (list 1)))
              (:end
               (let ((count (document-line-count document)))
                 (done (document-replace-lines document count count lines) (list (1+ count)))))
              ((:before :after)
               (aitools.kernel.domain:flow-call
                 (resolve-lines/k document selector path)
                 (:on-error (code message &key candidates diagnostics conflicts repairs path)
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
                 (:on-ranges (ranges)
                   (aitools.kernel.domain:flow-call
                     (check-expect-count/k context (length ranges))
                     (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
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
                     (:on-ok ()
                       (let ((positions
                              (mapcar
                               (lambda (range)
                                 (if (eq mode :before)
                                     (1- (car range))
                                     (cdr range)))
                               ranges))
                             (result document))
                         (dolist (position (reverse positions))
                           (setf result (document-replace-lines result position position lines)))
                         (done result
                               (loop for position in positions
                                     for index from 0
                                     collect (+ position 1 (* index (length lines))))))))))))))
        (:on-filtered ())))))

(define-write-command "insert" (ports env positionals options &key on-plan on-error)
  (let* ((path (first positionals))
         (at (getf options :at))
         (modes
          (remove nil (list (and at :at) (and (getf options :before) :before) (and (getf options :after) :after)))))
    (cond
      ((or (null path) (rest positionals)) (flow-continue :on-error "argument.invalid" "insert takes exactly one path"))
      ((/= (length modes) 1)
       (flow-continue :on-error "argument.invalid" "insert needs exactly one of --at, --before, --after"))
      ((and at (not (member at '("start" "end") :test #'string=)))
       (flow-continue :on-error
                      "argument.invalid"
                      (format
                        nil
                        "--at ~S must be start or end"
                        at)))
      (t
       (aitools.kernel.domain:flow-call
         (read-content-text/k ports options)
         (:on-error (code message)
           (flow-continue :on-error code message))
         (:on-text (text)
           (flet ((plan (mode selector)
                    (flow-continue
                      :on-plan
                      (make-write-plan
                       :command "insert"
                       :targets (list (make-write-target path))
                       :inputs (list text)
                       :guard-requirements (selector-guards selector path)
                       :expect-hashes (getf options :expect-hash)
                       :expect-count (getf options :expect-count)
                       :replayable
                       (and (content-selector-p selector)
                            (null (getf options :expect-hash)))
                       :plan (%insert-plan text mode selector)
                       :record-options (inline-content-options options text)
                       :record-positionals
                       (lambda (paths)
                         (list (first paths)))))))
             (aitools.kernel.domain:flow-call
              (parse-selector/k :insert options)
              (:on-error (code message &key repairs)
                (flow-continue :on-error code message :repairs repairs))
              (:on-selector (selector)
                (if at
                    (flow-continue :on-error "argument.invalid" "--at does not take a selector")
                    (plan (if (getf options :before) :before :after) selector)))
              (:on-none ()
                (if at
                    (plan (if (string= at "start") :start :end) nil)
                    (flow-continue
                     :on-error
                     "argument.invalid"
                     (concatenate
                      (quote string)
                      "--before/--after need a selector "
                      "(--range, --symbol, --between,"
                      " --match)"))))))))))))
