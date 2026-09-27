;;;; packages/feature/inspect/src/application/table-flows.lisp
;;;;
;;;; `table read` and `table agg`: one parse of the file into
;;;; a typed table, then row filtering (`--where`), column choice
;;;; (`--columns`), a row window (`--range`, `--limit`), or grouping.
;;;; Both join a tx's read set (`table read` is a single-file read;
;;;; `agg` reads the same way). This file holds the shared options and
;;;; parsing plus `table read`; the `table agg` flow is in
;;;; table-agg-flows.lisp.
(in-package #:aitools.inspect.application)

(defstruct (table-options (:constructor make-table-options) (:copier nil))
  "The reading options `table read` and `table agg` share."
  (format nil :read-only t)
  (delimiter nil :read-only t)
  (pointer nil :read-only t)
  (no-header nil :read-only t)
  (where '() :read-only t)
  (encoding nil :read-only t)
  (ws-columns nil :read-only t))

(defun %table-option-words (options)
  "The reading options as command words, for next_commands."
  (append (when (table-options-format options) (list "--format" (table-options-format options)))
          (when (table-options-delimiter options) (list "--delimiter" (table-options-delimiter options)))
          (when (table-options-pointer options) (list "--pointer" (table-options-pointer options)))
          (when (table-options-no-header options) (list "--no-header"))
          (when (table-options-ws-columns options)
            (list "--ws-columns" (princ-to-string (table-options-ws-columns options))))
          (loop for where in (table-options-where options) append (list "--where" where))
          (when (table-options-encoding options) (list "--encoding" (table-options-encoding options)))))

(defun %table-invalid (on-error message &optional (command "aitools schema table read"))
  (fail on-error "argument.invalid" message
        :repairs (list (repair "table-options" "See the table options and formats." command))))

(defun %validate-table-options (options on-valid on-error)
  (let ((format (table-options-format options)))
    (cond ((and format (not (member format *table-formats* :test #'string=)))
           (%table-invalid on-error (format nil "--format ~S is not one of ~{~A~^, ~}" format *table-formats*)))
          ((and (equal format "sep") (zerop (length (or (table-options-delimiter options) ""))))
           (%table-invalid on-error "--format sep needs a non-empty --delimiter"))
          ((and (table-options-delimiter options) (not (equal format "sep")))
           (%table-invalid on-error "--delimiter applies to --format sep only"))
          ((and (table-options-pointer options) (not (member format '(nil "json") :test #'equal)))
           (%table-invalid on-error "--pointer applies to --format json only"))
          ((and (table-options-ws-columns options) (not (equal format "ws")))
           (%table-invalid on-error "--ws-columns applies to --format ws only"))
          ((and (table-options-encoding options) (null (find-encoding (table-options-encoding options))))
           (fail on-error "input.unsupported-format"
                 (format nil "unknown encoding ~S; supported: ~{~A~^, ~}" (table-options-encoding options)
                         (mapcar #'encoding-name *supported-encodings*))
                 :repairs (list (repair "table-options" "See the supported encodings." "aitools schema table read"))))
          (t (funcall on-valid)))))

(defun %table-lines/k (octets options continuation)
  "CONTINUATION (lines) with OCTETS decoded per `--encoding`."
  (let ((encoding (and (table-options-encoding options) (find-encoding (table-options-encoding options)))))
    (if (and encoding (not (eq encoding :utf-8)))
        (decode-charset-lines/k octets encoding
                                :on-decoded (lambda (lines replacements)
                                              (declare (ignore replacements))
                                              (funcall continuation lines)))
        (funcall continuation (decode-text-lines octets)))))

(defun %parse-target-table/k (context target options on-table on-error)
  (multiple-value-bind (octets problem) (read-target-octets context target)
    (let ((path (file-target-argument target)))
    (if (null octets)
        (fail-target-read context target on-error problem)
        (%table-lines/k
         octets options
         (lambda (lines)
           (let ((format (if (table-options-format options)
                             (intern (string-upcase (table-options-format options)) :keyword)
                             (detect-table-format (file-target-absolute target) lines))))
             (if (null format)
                 (fail on-error "input.unsupported-format"
                       (format nil "cannot tell the table format of ~A; give --format" path)
                       :repairs (list (repair "whitespace-columns" "Split lines at whitespace runs."
                                              (command-line context (list "table" "read" path "--format" "ws")))
                                      (repair "whole-lines" "Read each line as one column."
                                              (command-line context (list "table" "read" path "--format" "lines")))))
                 (parse-table/k lines format
                                :delimiter (table-options-delimiter options)
                                :pointer (table-options-pointer options)
                                :no-header (table-options-no-header options)
                                :ws-columns (table-options-ws-columns options)
                                :on-table on-table
                                :on-error (lambda (message line)
                                            (fail on-error "input.syntax-error"
                                                  (format nil "~A: ~A at line ~D" path message line)
                                                  :diagnostics (list (json-object "line" line "col" 1 "message" message))
                                                  :repairs (list (repair "read-around" "Read the lines around the problem."
                                                                         (command-line context (list "read" path "--range"
                                                                                                     (format nil "~D:~D" (max 1 (- line 3)) (+ line 3)))))))))))))))))

(defun %filter-rows/k (table where on-rows on-error)
  "CONTINUATION ON-ROWS (rows) with TABLE's rows satisfying every WHERE text."
  (let ((comparisons '()))
    (dolist (text where)
      (table-comparison/k table text
                          :on-comparison (lambda (comparison) (push comparison comparisons))
                          :on-error (lambda (message)
                                      (return-from %filter-rows/k
                                        (%table-invalid on-error message)))))
    (let ((comparisons (nreverse comparisons)))
      (funcall on-rows (remove-if-not (lambda (row) (table-row-matches-p row comparisons)) (table-rows table))))))

(defun %call-with-table/k (ports path options &key root tx lock-timeout on-table on-error)
  "Validate OPTIONS, read and parse PATH, filter by `--where`, and call
ON-TABLE (context target table rows)."
  (declare (type function on-table on-error))
  (%validate-table-options
   options
   (lambda ()
     (call-with-inspect-file/k
      ports path :root root :tx tx :lock-timeout lock-timeout :record t :on-error on-error
      :on-file (lambda (context target)
                 (%parse-target-table/k
                  context target options
                  (lambda (table)
                    (%filter-rows/k table (table-options-where options)
                                    (lambda (rows) (funcall on-table context target table rows))
                                    on-error))
                  on-error))))
   on-error))

(defun %column-indexes/k (table specs on-indexes on-error)
  "Indexes for SPECS (names or 1-based numbers), all columns when SPECS is NIL."
  (if (null specs)
      (funcall on-indexes (loop for index below (length (table-columns table)) collect index))
      (let ((indexes (mapcar (lambda (spec) (table-column-index table spec)) specs)))
        (if (member nil indexes)
            (fail on-error "argument.invalid"
                  (format nil "no column ~S; columns: ~{~A~^, ~}" (nth (position nil indexes) specs) (table-columns table))
                  :candidates (mapcar (lambda (name) (json-object "column" name))
                                      (rank-similar (nth (position nil indexes) specs) (table-columns table)))
                  :repairs (list (repair "table-options" "Name columns as listed, or by 1-based number."
                                         "aitools schema table read")))
            (funcall on-indexes indexes)))))

;;; ------------------------------------------------------------ table read

(defun %row-window/k (range total on-window on-error)
  "ON-WINDOW (start end) for `--range` over TOTAL rows (END may be 0 rows)."
  (if (null range)
      (funcall on-window 1 total)
      (multiple-value-bind (start end) (handler-case (parse-range-spec range) (error () (values nil nil)))
        (if (null start)
            (%table-invalid on-error (format nil "--range ~S is not S:E, S:, or N (1-based rows)" range))
            (funcall on-window start (min total (or end total)))))))

(defun %table-read-fields (context path table rows indexes options start end limit)
  "(VALUES fields truncated)."
  (let* ((last (min end (+ start limit -1)))
         (shown (if (<= start last) (subseq rows (1- start) last) '()))
         (truncated (< last end))
         (words (append (list "table" "read" path) (%table-option-words options))))
    (values (append (list (cons "path" path)
                          (cons "format" (string-downcase (symbol-name (table-format table))))
                          (cons "columns" (mapcar (lambda (index)
                                                    (json-object "name" (nth index (table-columns table))
                                                                 "type" (nth index (table-types table))))
                                                  indexes))
                          (cons "start_row" start)
                          (cons "rows" (mapcar (lambda (row) (table-row-json row indexes)) shown))
                          (cons "total_rows" (length rows))
                          (cons "truncated" (json-bool truncated)))
                    (when truncated
                      (list (cons "next_commands"
                                  (list (command-line context
                                                      (append words
                                                              (list "--range" (format nil "~D:~D" (1+ last) (min end (+ last limit)))
                                                                    "--limit" (princ-to-string limit)))))))))
            truncated)))

(defun table-read-flow (ports path &key root tx lock-timeout format delimiter pointer no-header ws-columns columns
                                       where range encoding (limit 50) on-ok on-partial on-error)
  "`table read`. COLUMNS and WHERE are lists of texts. Calls ON-OK or ON-PARTIAL
(fields), or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (let ((options (make-table-options :format format :delimiter delimiter :pointer pointer :no-header no-header
                                     :ws-columns ws-columns :where where :encoding encoding)))
    (%call-with-table/k
     ports path options :root root :tx tx :lock-timeout lock-timeout :on-error on-error
     :on-table (lambda (context target table rows)
                 (declare (ignore target))
                 (%column-indexes/k
                  table columns
                  (lambda (indexes)
                    (%row-window/k
                     range (length rows)
                     (lambda (start end)
                       (multiple-value-bind (fields truncated)
                           (%table-read-fields context path table rows indexes options start end limit)
                         (funcall (if truncated on-partial on-ok) fields)))
                     on-error))
                  on-error)))))
