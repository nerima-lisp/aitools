(in-package #:aitools.edit.application)

(defun %parse-to-position (text)
  "(values kind argument) of --to-position TEXT, or NIL."
  (cond
    ((string= text "start") (values :start nil))
    ((string= text "end") (values :end nil))
    (t
     (let ((colon (position #\: text)))
       (when colon
         (let ((kind (subseq text 0 colon))
               (argument (subseq text (1+ colon))))
           (cond
             ((and (member kind '("after" "before") :test #'string=)
                   (parse-count argument)
                   (plusp (parse-count argument)))
              (values
               (if (string= kind "after")
                   :after
                   :before)
               (parse-count argument)))
             ((and (member kind '("after-symbol" "before-symbol") :test #'string=) (plusp (length argument)))
              (values
               (if (string= kind "after-symbol")
                   :after-symbol
                   :before-symbol)
               argument)))))))))

(aitools.kernel.domain:define-flow
  (%insertion-index/k document path kind argument &key on-error on-index)
  "The 0-based line index of DOCUMENT where moved lines go."
  (let ((count (document-line-count document)))
    (ecase kind
      (:start
       (aitools.kernel.domain:flow-continue
         :on-index
         0))
      (:end
       (aitools.kernel.domain:flow-continue
         :on-index
         count))
      ((:after :before)
       (if (> argument count)
           (aitools.kernel.domain:flow-continue
             :on-error
             "selection.no-match"
             (format
               nil
               "~A has ~D lines; line ~D does not exist"
               path
               count
               argument)
             :candidates
             (list
              (json-object "line"
                           count
                           "text"
                           (if (plusp count)
                               (document-line document (1- count))
                               ""))))
           (aitools.kernel.domain:flow-continue
             :on-index
             (if (eq kind :after)
                 argument
                 (1- argument)))))
      ((:after-symbol :before-symbol)
       (aitools.kernel.domain:flow-call
         (resolve-lines/k document (aitools.kernel.domain:make-symbol-selector argument) path)
         (:on-error (code message &key candidates diagnostics conflicts repairs path)
           (flow-continue :on-error
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
         (:on-ranges (ranges)
           (let ((range (first ranges)))
             (aitools.kernel.domain:flow-continue
               :on-index
               (if (eq kind :after-symbol)
                   (cdr range)
                   (1- (car range)))))))))))

(defun %move-lines-plan (selector kind argument same-file)
  (lambda (context &key on-committed on-rejected)
    (let ((source (context-path context 0))
          (destination (context-path context 1)))
      (aitools.kernel.domain:flow-call
        (read-document/k context source)
        (:on-error (code message &key candidates diagnostics conflicts repairs ((:path ignored-path)))
          (declare (ignore ignored-path))
          (funcall on-rejected
                   code message
                   :candidates candidates
                   :diagnostics diagnostics
                   :conflicts conflicts
                   :repairs repairs))
        (:on-filtered ())
        (:on-document (document)
          (aitools.kernel.domain:flow-call
            (resolve-lines/k document selector source)
            (:on-error (code message &key candidates diagnostics conflicts repairs path)
              (declare (ignore path))
              (funcall on-rejected
                       code message
                       :candidates candidates
                       :diagnostics diagnostics
                       :conflicts conflicts
                       :repairs repairs))
            (:on-ranges (ranges)
              (aitools.kernel.domain:flow-call
                (check-expect-count/k context
                  (reduce #'+ ranges
                       :key
                       (lambda (range)
                         (1+ (- (cdr range) (car range))))))
                (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                  (declare (ignore path))
                  (funcall on-rejected
                          code message
                          :candidates candidates
                          :diagnostics diagnostics
                          :conflicts conflicts
                          :repairs repairs))
                (:on-ok ()
                  (let* ((moved
                         (loop for range in ranges
                               append
                                 (loop for line from (car range) to (cdr range)
                                       collect (document-line document (1- line)))))
                        (removed
                         (let ((result document))
                           (dolist (range (reverse ranges) result)
                             (setf result
                                   (document-replace-lines
                                    result
                                    (1- (car range))
                                    (cdr range)
                                    '()))))))
                   (if (or same-file (string= source destination))
                       (aitools.kernel.domain:flow-call
                         (%insertion-index/k document source kind argument)
                         (:on-error
                           (code message &key candidates diagnostics conflicts repairs path)
                           (declare (ignore path))
                           (funcall on-rejected
                                    code message
                                    :candidates candidates
                                    :diagnostics diagnostics
                                    :conflicts conflicts
                                    :repairs repairs))
                         (:on-index (index)
                           (if (some
                                 (lambda (range)
                                   (< (1- (car range)) index (cdr range)))
                                 ranges)
                               (funcall on-rejected
                                        "argument.invalid"
                                        "--to-position falls inside the lines being moved")
                               (let ((shift
                                       (loop for range in ranges
                                             when (<= (cdr range) index)
                                               sum
                                                 (1+ (- (cdr range) (car range))))))
                                 (aitools.kernel.domain:flow-call
                                   (commit-document
                                     context
                                     source
                                     (document-replace-lines
                                       removed
                                       (- index shift)
                                       (- index shift)
                                       moved))
                                   (:on-committed (requests &optional extra)
                                     (funcall on-committed requests extra)))))))
                       (flet ((write-both (target-document)
                                (aitools.kernel.domain:flow-call
                                  (%insertion-index/k target-document destination kind argument)
                                  (:on-error
                                    (code message &key candidates diagnostics conflicts repairs path)
                                    (declare (ignore path))
                                    (funcall on-rejected
                                             code message
                                             :candidates candidates
                                             :diagnostics diagnostics
                                             :conflicts conflicts
                                             :repairs repairs))
                                  (:on-index (index)
                                    (funcall on-committed
                                             (list
                                               (write-document-request source removed)
                                               (write-document-request
                                                 destination
                                                 (document-replace-lines
                                                   target-document
                                                   index
                                                   index
                                                   moved)))))))
                         )
                         ;; A missing destination starts empty. Only --to-position
                         ;; start or end can reach one: a position needs the
                         ;; destination's --expect-hash, which no missing file matches.
                         (if (aitools.store.domain:entry-state-absent-p
                               (aitools.store.application:view-path-state
                                 (write-context-view context)
                                 destination))
                             (write-both
                               (make-text-document
                                 ""
                                 :eol
                                 (text-document-eol document)))
                             (aitools.kernel.domain:flow-call
                               (read-document/k context destination)
                               (:on-error
                                 (code message &key candidates diagnostics conflicts repairs
                                       ((:path ignored-path)))
                                 (declare (ignore ignored-path))
                                 (funcall on-rejected
                                          code message
                                          :candidates candidates
                                          :diagnostics diagnostics
                                          :conflicts conflicts
                                          :repairs repairs))
                               (:on-document (document)
                                 (funcall #'write-both document))
                               (:on-filtered ())))))
              )))
            )
            ))))))
