;;;; packages/feature/inspect/src/application/table-option-flows.lisp
(in-package #:aitools.inspect.application)
(defstruct (table-options (:constructor make-table-options) (:copier nil))
  "The reading options for table read and table agg are shared."
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
(aitools.kernel.domain:define-flow
 (%table-invalid message &key (command "aitools schema table read")
                        on-error)
 (aitools.kernel.domain:flow-continue
  :on-error
  "argument.invalid"
  message
  :repairs
  (list (repair "table-options"
                "See the table options and formats."
                command))))
(aitools.kernel.domain:define-flow
 (%validate-table-options options &key on-valid on-error)
 (let ((format (table-options-format options)))
   (cond
     ((and format
           (not (member format *table-formats* :test #'string=)))
      (aitools.kernel.domain:flow-call
          (%table-invalid
           (format nil "--format ~S is not one of ~{~A~^, ~}"
                   format *table-formats*))
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message :repairs repairs))))
     ((and (equal format "sep")
           (zerop (length (or (table-options-delimiter options) ""))))
      (aitools.kernel.domain:flow-call
          (%table-invalid "--format sep needs a non-empty --delimiter")
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message :repairs repairs))))
     ((and (table-options-delimiter options)
           (not (equal format "sep")))
      (aitools.kernel.domain:flow-call
          (%table-invalid "--delimiter applies to --format sep only")
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message :repairs repairs))))
     ((and (table-options-pointer options)
           (not (member format '(nil "json") :test #'equal)))
      (aitools.kernel.domain:flow-call
          (%table-invalid "--pointer applies to --format json only")
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message :repairs repairs))))
     ((and (table-options-ws-columns options)
           (not (equal format "ws")))
      (aitools.kernel.domain:flow-call
          (%table-invalid "--ws-columns applies to --format ws only")
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message :repairs repairs))))
     ((and (table-options-encoding options)
           (null (find-encoding (table-options-encoding options))))
      (aitools.kernel.domain:flow-continue
       :on-error
       "input.unsupported-format"
       (format nil "unknown encoding ~S; supported: ~{~A~^, ~}"
               (table-options-encoding options)
               (mapcar #'encoding-name *supported-encodings*))
       :repairs
       (list (repair "table-options"
                     "See the supported encodings."
                     "aitools schema table read"))))
     (t
      (aitools.kernel.domain:flow-continue
       :on-valid)))))
(aitools.kernel.domain:define-flow
 (%table-lines/k octets options &key on-lines)
 "ON-LINES (lines) with OCTETS decoded per --encoding."
 (declare (type function on-lines))
 (let ((encoding
         (and (table-options-encoding options)
              (find-encoding (table-options-encoding options)))))
   (if (and encoding (not (eq encoding :utf-8)))
       (aitools.kernel.domain:flow-call
           (decode-charset-lines/k octets encoding)
         (:on-decoded (lines replacements)
           (declare (ignore replacements))
           (aitools.kernel.domain:flow-continue
            :on-lines lines)))
       (aitools.kernel.domain:flow-continue
        :on-lines
        (decode-text-lines octets)))))
