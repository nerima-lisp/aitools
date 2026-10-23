;;;; packages/feature/edit/src/application/json-flows.lisp
;;;;
;;;; The `json` write commands and `table set`.
;;;; A JSON write keeps key order, re-indents with the width the file
;;;; already uses, and keeps the file's BOM, line ending and final newline.
(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
 (%read-json/k context path &key on-error on-json)
 "PATH's JSON: ON-JSON (value document indent), or input.unsupported-format
when it is not JSON."
  (aitools.kernel.domain:flow-call
   (read-document/k context path)
   (:on-error (code message &key candidates diagnostics conflicts repairs path)
     (aitools.kernel.domain:flow-continue
       :on-error
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
   (:on-document (document)
     (let ((text (document-logical-text document)))
       (aitools.kernel.domain:flow-call
         (parse-json-text/k text)
         (:on-value (value)
           (aitools.kernel.domain:flow-continue
             :on-json
             value
             document
             (detect-json-indent text)))
         (:on-invalid (message)
           (aitools.kernel.domain:flow-continue
             :on-error
             "input.unsupported-format"
             (format
               nil
               "~A is not JSON: ~A"
               path
               message))))))
   (:on-filtered ())))

(defun %json-document (document value &key indent sort-keys)
  "DOCUMENT's layout holding VALUE serialized."
  (let* ((text (serialize-json value :indent indent :sort-keys sort-keys))
         (result
          (make-text-document text
                              :bom-p
                              (text-document-bom-p document)
                              :eol
                              (text-document-eol document))))
    (document-with-final-newline
     (document-with-eol result (text-document-eol document))
     (or (document-final-newline-p document)
         (zerop (document-line-count document))))))

(defun %json-repairs (path code repair-pointer)
  "contract-F3: repairs for a JSON write failure that mirror `json get`
rather than the generic file repairs. REPAIR-POINTER is the command's pointer
tokens (set/delete) or NIL (merge/patch/fmt)."
  (let ((file (aitools.protocol.domain:shell-quote path)))
    (flet ((get-cmd (tokens)
             (if tokens
                 (format nil
                         "aitools json get ~A ~A"
                         file
                         (aitools.protocol.domain:shell-quote
                          (format-json-pointer tokens)))
                 (format nil "aitools json get ~A" file))))
      (if (and repair-pointer (string= code "input.not-found"))
          (list
           (repair "list-keys"
                   "List the keys or elements at the pointer's parent."
                   (get-cmd (butlast repair-pointer))))
          (list
           (repair "read-json"
                   "Read the JSON value to choose a current pointer."
                   (get-cmd repair-pointer)))))))

(defun %json-plan (transform &key indent-function sort-keys repair-pointer
                             reader document-function repairs-function
                             extra-function)
  "Apply TRANSFORM through the shared JSON value model and write pipeline."
  (lambda (context &key on-committed on-rejected
         &aux (path (context-path context)))
    (funcall
     (or reader (function %read-json/k))
     context
     path
     :on-error
     (lambda (code message &key candidates diagnostics conflicts repairs
                   ((:path ignored-path)))
       (declare (ignore ignored-path))
       (funcall on-rejected code
                message
                :candidates candidates
                :diagnostics diagnostics
                :conflicts conflicts
                :repairs repairs))
     :on-json
     (lambda (value document metadata)
       (handler-case
           (aitools.kernel.domain:flow-call
            (commit-document
             context
             path
             (if document-function
                 (funcall document-function
                          document
                          (funcall transform value)
                          metadata)
               (%json-document document
                               (funcall transform value)
                               :indent
                               (if indent-function
                                   (funcall indent-function metadata)
                                 metadata)
                               :sort-keys
                               sort-keys))
             :extra (when extra-function
                      (funcall extra-function metadata)))
            (:on-committed (requests &optional extra)
                           (funcall on-committed requests extra)))
         (edit-refusal (condition)
                       (let ((code (edit-refusal-code condition)))
                         (funcall on-rejected
                                  code
                                  (edit-refusal-detail condition)
                                  :repairs
                                  (if repairs-function
                                      (funcall repairs-function
                                               (write-context-command context)
                                               path
                                               code
                                               repair-pointer)
                                    (%json-repairs path code repair-pointer))))))))))
