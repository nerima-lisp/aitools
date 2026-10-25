;;;;
;;;; The read side of the `json` group: `json get`
;;;; (the `jq '.a.b'`, `keys`, `length`, `-r` replacement), `json select`
;;;; (`select`, `sort_by`, `map | length`), and `json diff`. A file that is
;;;; not JSON is `input.unsupported-format`; `get` and `select` join a tx's
;;;; read set.
(in-package #:aitools.inspect.application)

(defun %utf8-char-length (char)
  (let ((code (char-code char)))
    (cond
      ((< code #x80) 1)
      ((< code #x800) 2)
      ((< code #x10000) 3)
      (t 4))))

(defun %utf8-length (string)
  (loop for char across string
        sum (%utf8-char-length char)))

(defun %utf8-prefix (string max-bytes)
  (loop for char across string
        for
        end from 0
        sum (%utf8-char-length char) into bytes
        when (> bytes max-bytes)
          return (subseq string 0 end)
        finally (return string)))

(aitools.kernel.domain:define-flow
 (%json-document context target &key on-document on-error)
 "Parse TARGET's bytes as JSON and call ON-DOCUMENT (value)."
 (declare (type function on-document on-error))
 (multiple-value-bind (octets problem)
     (read-target-octets context target)
   (if (null octets)
       (aitools.kernel.domain:flow-call
        (%read-target-error/k context target problem)
        (:on-error (code message &key candidates diagnostics conflicts repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message
           :candidates candidates
           :diagnostics diagnostics
           :conflicts conflicts
           :repairs repairs)))
       (aitools.kernel.domain:flow-call
        (parse-json-document/k
         (%join-lines (decode-text-lines octets)))
        (:on-value (document)
          (aitools.kernel.domain:flow-continue
           :on-document document))
        (:on-error (message line column)
          (aitools.kernel.domain:flow-continue
           :on-error
           "input.unsupported-format"
           (format nil "~A is not JSON: ~A at line ~D, column ~D"
                   (file-target-argument target)
                   message
                   line
                   column)
           :diagnostics
           (list (json-object "line" line "col" column "message" message))
           :repairs
           (list
            (repair "check"
                    "Locate the syntax error."
                    (command-line context
                                  (list "check"
                                        (file-target-argument target)
                                        "--format"
                                        "json"))))))))))

(aitools.kernel.domain:define-flow
 (%pointer-tokens pointer group &key on-tokens on-error)
 (declare (type function on-tokens on-error))
 (let ((tokens (parse-json-pointer pointer)))
   (if (eq tokens :invalid)
       (aitools.kernel.domain:flow-continue
        :on-error
        "argument.invalid"
        (format nil
                "~S is not a JSON pointer (\"\" or /a/b, with ~~0 for ~~ and ~~1 for /)"
                pointer)
        :repairs
        (list
         (repair "whole-document"
                 "Start from the document's top-level keys."
                 (format nil "aitools schema ~A get" group))))
       (aitools.kernel.domain:flow-continue :on-tokens tokens))))

(aitools.kernel.domain:define-flow
 (%resolve-or-fail context target document tokens group &key on-found on-error)
 (declare (type function on-found on-error))
 (aitools.kernel.domain:flow-call
  (resolve-json-pointer/k document tokens)
  (:on-found (value)
    (aitools.kernel.domain:flow-continue :on-found value))
  (:on-missing (parent-tokens parent token)
    (let ((parent-pointer (format-json-pointer parent-tokens)))
      (aitools.kernel.domain:flow-continue
       :on-error
       "input.not-found"
       (format nil
               "~A has no ~A (its deepest existing prefix is ~S)"
               (file-target-argument target)
               (format-json-pointer tokens)
               parent-pointer)
       :candidates
       (mapcar (lambda (name)
                 (json-object "pointer"
                              (format-json-pointer
                               (append parent-tokens (list name)))))
               (rank-similar token (json-child-names parent) :count 5))
       :repairs
       (list
        (repair "list-keys"
                "List the keys that exist there."
                (command-line context
                              (list group
                                    "get"
                                    (file-target-argument target)
                                    parent-pointer
                                    "--keys")))))))))

(aitools.kernel.domain:define-flow
 (%max-bytes text group &key on-bytes on-error)
 (declare (type function on-bytes on-error))
 (let ((bytes (handler-case (size-bytes (parse-size text))
                (error () nil))))
   (if (and bytes (plusp bytes))
       (aitools.kernel.domain:flow-continue :on-bytes bytes)
       (aitools.kernel.domain:flow-continue
        :on-error
        "argument.invalid"
        (format nil
                "--max-bytes ~S is not a size (<n>, <n>KiB, <n>MiB)"
                text)
        :repairs
        (list
         (repair "default-size"
                 "Use the default limit."
                 (format nil "aitools schema ~A get" group)))))))

(defun %json-get-fields (context target group pointer value keys raw max-bytes)
  "(VALUES fields truncated) for `json get`."
  (let* ((head
          (list (cons "path" (file-target-argument target))
                (cons "pointer" pointer)
                (cons "type" (json-type-name value))
                (cons "length" (json-or-null (json-value-length value)))))
         (body
          (cond
            (keys (list (cons "keys" (json-child-names value))))
            (raw
             (list
              (cons "text"
                    (if (stringp value)
                        value
                        (render-json value)))))
            (t (list (cons "value" value)))))
         (rendered (render-json (cdr (first body)))))
    (if (<= (%utf8-length rendered) max-bytes)
        (values
         (append head
                 body
                 (list (cons "truncated" (json-false))
                       (cons "approx_tokens"
                             (approx-token-count (length rendered)))))
         nil)
        (let ((preview (%utf8-prefix rendered max-bytes)))
          (values
           (append head
                   (list (cons "value_preview" preview)
                         (cons "truncated" t)
                         (cons "approx_tokens"
                               (approx-token-count (length preview)))
                         (cons "next_commands"
                               (remove nil
                                       (list
                                        (and (not keys)
                                             (member (json-type-name value)
                                                     '("object" "array")
                                                     :test
                                                     #'string=)
                                             (command-line context
                                                           (list group
                                                                 "get"
                                                                 (file-target-argument
                                                                  target)
                                                                 pointer
                                                                 "--keys")))
                                        (and (json-array-value-p value)
                                             (plusp (length value))
                                             (command-line context
                                                           (list group
                                                                 "get"
                                                                 (file-target-argument
                                                                  target)
                                                                 (concatenate
                                                                  'string
                                                                  pointer
                                                                  "/0")))))))))
           t)))))
