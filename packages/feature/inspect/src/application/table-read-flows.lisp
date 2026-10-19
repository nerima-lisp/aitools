;;;; packages/feature/inspect/src/application/table-read-flows.lisp
(in-package #:aitools.inspect.application)
;;; ------------------------------------------------------------ table read

(aitools.kernel.domain:define-flow
 (%row-window/k range total &key on-window on-error)
 "ON-WINDOW (start end) for --range over TOTAL rows; END may be zero."
 (declare (type function on-window on-error))
 (if (null range)
     (aitools.kernel.domain:flow-continue
      :on-window 1 total)
     (multiple-value-bind (start end)
         (handler-case
             (parse-range-spec range)
           (error ()
             (values nil nil)))
       (if (null start)
           (aitools.kernel.domain:flow-call
               (%table-invalid
                (format nil
                        "--range ~S is not S:E, S:, or N (1-based rows)"
                        range))
             (:on-error (code message &key repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message :repairs repairs)))
           (aitools.kernel.domain:flow-continue
            :on-window start (min total (or end total)))))))
(defun %table-read-fields (context path table rows indexes options start end limit)
  "(VALUES fields truncated)."
  (let* ((last (min end (+ start limit -1)))
         (shown (if (<= start last) (subseq rows (1- start) last) '()))
         (truncated (< last end))
         (words (append (list "table" "read" path) (%table-option-words options)))
         (next-command
           (when truncated
             (command-line
              context
              (append words
                      (list "--range"
                            (format nil "~D:~D"
                                    (1+ last)
                                    (min end (+ last limit)))
                            "--limit"
                            (princ-to-string limit))))))
         (fields
           (append
            (list (cons "path" path)
                  (cons "format" (string-downcase (symbol-name (table-format table))))
                  (cons "columns"
                        (mapcar (lambda (index)
                                  (json-object
                                   "name" (nth index (table-columns table))
                                   "type" (nth index (table-types table))))
                                indexes))
                  (cons "start_row" start)
                  (cons "rows"
                        (mapcar (lambda (row)
                                  (table-row-json row indexes))
                                shown))
                  (cons "total_rows" (length rows))
                  (cons "truncated" (json-bool truncated)))
            (when next-command
              (list (cons "next_commands" (list next-command)))))))
    (values fields truncated)))
(aitools.kernel.domain:define-flow
 (table-read-flow ports path
                  &key root tx lock-timeout format delimiter pointer no-header
                    ws-columns columns where range encoding (limit 50)
                    on-ok on-partial on-error)
 "The table read command. COLUMNS and WHERE are lists of texts. Calls ON-OK
or ON-PARTIAL (fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (let ((options
         (make-table-options
          :format format
          :delimiter delimiter
          :pointer pointer
          :no-header no-header
          :ws-columns ws-columns
          :where where
          :encoding encoding)))
   (aitools.kernel.domain:flow-call
       (%call-with-table/k
        ports path options
        :root root
        :tx tx
        :lock-timeout lock-timeout)
     (:on-error
      (code message &key candidates diagnostics conflicts repairs)
       (aitools.kernel.domain:flow-continue
        :on-error
        code message
        :candidates candidates
        :diagnostics diagnostics
        :conflicts conflicts
        :repairs repairs))
     (:on-table (context target table rows)
       (declare (ignore target))
       (aitools.kernel.domain:flow-call
           (%column-indexes/k table columns)
         (:on-error (code message &key candidates repairs)
           (aitools.kernel.domain:flow-continue
            :on-error
            code message
            :candidates candidates
            :repairs repairs))
         (:on-indexes (indexes)
           (aitools.kernel.domain:flow-call
               (%row-window/k range (length rows))
             (:on-error (code message &key repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message :repairs repairs))
             (:on-window (start end)
               (multiple-value-bind (fields truncated)
                   (%table-read-fields
                    context path table rows indexes options start end limit)
                 (if truncated
                     (aitools.kernel.domain:flow-continue
                      :on-partial fields)
                     (aitools.kernel.domain:flow-continue
                      :on-ok fields)))))))))))
