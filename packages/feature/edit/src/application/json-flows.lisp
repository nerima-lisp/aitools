;;;; packages/feature/edit/src/application/json-flows.lisp
;;;;
;;;; The `json` write commands and `table set`.
;;;; A JSON write keeps key order, re-indents with the width the file
;;;; already uses, and keeps the file's BOM, line ending and final newline.
(in-package #:aitools.edit.application)

(defun %read-json/k (context path reject on-json)
  "PATH's JSON: ON-JSON (value document indent), or input.unsupported-format
when it is not JSON."
  (declare (type function reject on-json))
  (read-document/k context path reject
                   (lambda (document)
                     (let ((text (document-logical-text document)))
                       (parse-json-text/k text
                                          :on-value (lambda (value) (funcall on-json value document (detect-json-indent text)))
                                          :on-invalid (lambda (message)
                                                        (funcall reject "input.unsupported-format"
                                                                 (format nil "~A is not JSON: ~A" path message))))))))

(defun %json-document (document value &key indent sort-keys)
  "DOCUMENT's layout holding VALUE serialized."
  (let* ((text (serialize-json value :indent indent :sort-keys sort-keys))
         (result (make-text-document text :bom-p (text-document-bom-p document) :eol (text-document-eol document))))
    (document-with-final-newline (document-with-eol result (text-document-eol document))
                                 (or (document-final-newline-p document) (zerop (document-line-count document))))))

(defun %json-repairs (path code repair-pointer)
  "contract-F3: repairs for a JSON write failure that mirror `json get`
rather than the generic file repairs. REPAIR-POINTER is the command's pointer
tokens (set/delete) or NIL (merge/patch/fmt)."
  (let ((file (aitools.protocol.domain:shell-quote path)))
    (flet ((get-cmd (tokens)
             (if tokens
                 (format nil "aitools json get ~A ~A" file
                         (aitools.protocol.domain:shell-quote (format-json-pointer tokens)))
                 (format nil "aitools json get ~A" file))))
      (if (and repair-pointer (string= code "input.not-found"))
          (list (repair "list-keys" "List the keys or elements at the pointer's parent."
                        (get-cmd (butlast repair-pointer))))
          (list (repair "read-json" "Read the JSON value to choose a current pointer."
                        (get-cmd repair-pointer)))))))

(defun %json-plan (transform &key indent-function sort-keys repair-pointer)
  "A plan applying TRANSFORM (value -> value) to the target's JSON.
INDENT-FUNCTION maps the detected indent to the one written. A pointer
refusal from TRANSFORM (input.not-found, selection.no-match) is rejected with
pointer-aware repairs (contract-F3)."
  (lambda (context commit reject)
    (let ((path (context-path context)))
      (%read-json/k context path reject
                    (lambda (value document indent)
                      (handler-case
                          (commit-document context path
                                           (%json-document document (funcall transform value)
                                                           :indent (if indent-function (funcall indent-function indent) indent)
                                                           :sort-keys sort-keys)
                                           commit)
                        (edit-refusal (condition)
                          (funcall reject (edit-refusal-code condition) (edit-refusal-detail condition)
                                   :repairs (%json-repairs path (edit-refusal-code condition) repair-pointer)))))))))

(define-write-command "json.set" (ports env positionals options on-plan fail)
  (let ((path (first positionals)))
    (flet ((plan (pointer value record-options record-positionals inputs)
             (let ((tokens (handler-case (parse-json-pointer pointer)
                             (edit-refusal (condition)
                               (return-from plan (funcall fail (edit-refusal-code condition) (edit-refusal-detail condition)))))))
               (funcall on-plan
                        (make-write-plan
                         :command "json.set"
                         :targets (list (make-write-target path))
                         :inputs inputs
                         :expect-hashes (getf options :expect-hash)
                         :replayable (null (getf options :expect-hash))
                         :plan (%json-plan (lambda (document) (json-add document tokens value :array-mode :replace))
                                           :repair-pointer tokens)
                         :record-options record-options
                         :record-positionals (lambda (paths) (list* (first paths) record-positionals)))))))
      (cond
        ((null path) (funcall fail "argument.invalid" "json set needs PATH"))
        ((or (getf options :stdin) (getf options :stdin-data))
         (if (rest positionals)
             (funcall fail "argument.invalid" "json set --stdin reads {\"pointer\",\"value\"}; pass only PATH")
             (read-stdin-json/k ports options :on-error fail
                                :on-json (lambda (input text)
                                           (let ((pointer (cdr (%json-member input "pointer")))
                                                 (value (%json-member input "value")))
                                             (if (and (stringp pointer) value)
                                                 (plan pointer (cdr value) (inline-stdin-options options text) '() (list text))
                                                 (funcall fail "argument.invalid"
                                                          "json set --stdin reads {\"pointer\": string, \"value\": any}")))))))
        ((/= (length positionals) 3) (funcall fail "argument.invalid" "json set takes PATH POINTER VALUE (or --stdin)"))
        (t
         (let* ((text (third positionals))
                (value (parse-json-text/k
                        text
                        :on-value #'identity
                        :on-invalid (lambda (message)
                           (declare (ignore message))
                           (return-from prepare
                             (funcall fail "argument.invalid"
                                      (format nil "VALUE ~S is not JSON; a string needs JSON quotes" text)
                                      :repairs (list (repair "quote-string" "Pass the value as a JSON string."
                                                             (command-line (list "json" "set" path (second positionals)
                                                                                 (json-string-literal text)))))))))))
           (plan (second positionals) value options (rest positionals) (list text))))))))

(define-write-command "json.delete" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 2)
      (funcall fail "argument.invalid" "json delete takes PATH POINTER")
      (let ((tokens (handler-case (parse-json-pointer (second positionals))
                      (edit-refusal (condition)
                        (return-from prepare
                          (funcall fail (edit-refusal-code condition) (edit-refusal-detail condition)))))))
        (funcall on-plan
                 (make-write-plan
                  :command "json.delete"
                  :targets (list (make-write-target (first positionals)))
                  :expect-hashes (getf options :expect-hash)
                  :replayable (null (getf options :expect-hash))
                  :plan (%json-plan (lambda (document) (json-remove document tokens)) :repair-pointer tokens)
                  :record-options options
                  :record-positionals (lambda (paths) (list (first paths) (second positionals))))))))

(defun %json-stdin-command (name ports positionals options on-plan fail transform-for)
  "json merge / json patch: PATH plus a JSON document on --stdin, applied
by (TRANSFORM-FOR input) -> (value -> value)."
  (cond
    ((/= (length positionals) 1) (funcall fail "argument.invalid" (format nil "~A takes exactly one PATH" (command-display-name name))))
    ((not (or (getf options :stdin) (getf options :stdin-data)))
     (funcall fail "argument.invalid" (format nil "~A reads the patch from --stdin (stdin is never read implicitly)"
                                              (command-display-name name))))
    (t (read-stdin-json/k ports options :on-error fail
                          :on-json (lambda (input text)
                                     (funcall on-plan
                                              (make-write-plan
                                               :command name
                                               :targets (list (make-write-target (first positionals)))
                                               :inputs (list text)
                                               :expect-hashes (getf options :expect-hash)
                                               :replayable (null (getf options :expect-hash))
                                               :plan (%json-plan (funcall transform-for input))
                                               :record-options (inline-stdin-options options text)
                                               :record-positionals (lambda (paths) (list (first paths))))))))))

(define-write-command "json.merge" (ports env positionals options on-plan fail)
  (%json-stdin-command "json.merge" ports positionals options on-plan fail
                       (lambda (patch) (lambda (document) (json-merge-patch document patch)))))

(define-write-command "json.patch" (ports env positionals options on-plan fail)
  (%json-stdin-command "json.patch" ports positionals options on-plan fail
                       (lambda (operations) (lambda (document) (json-apply-patch document operations)))))

(define-write-command "json.fmt" (ports env positionals options on-plan fail)
  (let ((indent (getf options :indent)))
    (cond
      ((/= (length positionals) 1) (funcall fail "argument.invalid" "json fmt takes exactly one PATH"))
      ((and indent (not (and (parse-count indent) (<= (parse-count indent) 16))))
       (funcall fail "argument.invalid" (format nil "--indent ~S must be 0 to 16" indent)))
      ((and indent (getf options :minify)) (funcall fail "argument.invalid" "--indent and --minify cannot be combined"))
      (t
       (funcall on-plan
                (make-write-plan
                 :command "json.fmt"
                 :targets (list (make-write-target (first positionals)))
                 :expect-hashes (getf options :expect-hash)
                 :replayable (null (getf options :expect-hash))
                 :plan (%json-plan #'identity
                                   :sort-keys (getf options :sort-keys)
                                   :indent-function (lambda (detected)
                                                      (cond ((getf options :minify) nil)
                                                            (indent (make-string (parse-count indent) :initial-element #\Space))
                                                            (t (or detected "  ")))))
                 :record-options options
                 :record-positionals (lambda (paths) (list (first paths)))))))))

;;; ----------------------------------------------------------------- table set

(define-write-command "table.set" (ports env positionals options on-plan fail)
  (let ((path (first positionals)))
    (flet ((plan (row column value record-options inputs)
             (let ((delimiter (table-delimiter-for-path path))
                   (row-number (parse-count row)))
               (cond
                 ((null delimiter)
                  (funcall fail "input.unsupported-format" (format nil "table set edits .csv and .tsv files, not ~A" path)))
                 ((not (and row-number (plusp row-number)))
                  (funcall fail "argument.invalid" (format nil "--row ~S must be a positive integer" row)))
                 ((not (and (stringp column) (plusp (length column)) (stringp value)))
                  (funcall fail "argument.invalid" "table set needs --row, --column and --value"))
                 (t
                  (funcall on-plan
                           (make-write-plan
                            :command "table.set"
                            :targets (list (make-write-target path))
                            :inputs inputs
                            :guard-requirements (list (list :expect-hash path))
                            :expect-hashes (getf options :expect-hash)
                            :plan (lambda (context commit reject)
                                    (read-document/k
                                     context (context-path context) reject
                                     (lambda (document)
                                       (multiple-value-bind (text previous)
                                           (table-set-cell (document-text document) delimiter row-number column value)
                                         (commit-document context (context-path context)
                                                          (make-text-document text :bom-p (text-document-bom-p document)
                                                                                   :eol (text-document-eol document))
                                                          commit
                                                          (list (cons "previous" previous)))))))
                            :record-options record-options
                            :record-positionals (lambda (paths) (list (first paths))))))))))
      (cond
        ((or (null path) (rest positionals)) (funcall fail "argument.invalid" "table set takes exactly one PATH"))
        ((or (getf options :stdin) (getf options :stdin-data))
         (read-stdin-json/k ports options :on-error fail
                            :on-json (lambda (input text)
                                       (let ((row (cdr (%json-member input "row")))
                                             (column (cdr (%json-member input "column")))
                                             (value (cdr (%json-member input "value"))))
                                         (plan (if (json-num-p row) (json-num-text row) row)
                                               (if (json-num-p column) (json-num-text column) column)
                                               (if (json-num-p value) (json-num-text value) value)
                                               (inline-stdin-options options text) (list text))))))
        (t (plan (getf options :row) (getf options :column) (getf options :value) options
                 (list (or (getf options :value) ""))))))))
