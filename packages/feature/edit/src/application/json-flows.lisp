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

;;; ----------------------------------------------------------------- table set

