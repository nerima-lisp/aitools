;;;; packages/feature/inspect/src/application/json-flows.lisp
;;;;
;;;; The read side of the `json` group: `json get`
;;;; (the `jq '.a.b'`, `keys`, `length`, `-r` replacement), `json select`
;;;; (`select`, `sort_by`, `map | length`), and `json diff`. A file that is
;;;; not JSON is `input.unsupported-format`; `get` and `select` join a tx's
;;;; read set.
(in-package #:aitools.inspect.application)

(defun %utf8-length (string)
  (loop for char across string
        sum (let ((code (char-code char)))
              (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))))

(defun %json-document/k (context target on-document on-error)
  "Parse TARGET's bytes as JSON and call ON-DOCUMENT (value)."
  (multiple-value-bind (octets problem) (read-target-octets context target)
    (if (null octets)
        (fail-target-read context target on-error problem)
        (parse-json-document/k
         (%join-lines (decode-text-lines octets))
         :on-value on-document
         :on-error (lambda (message line column)
                     (fail on-error "input.unsupported-format"
                           (format nil "~A is not JSON: ~A at line ~D, column ~D"
                                   (file-target-argument target) message line column)
                           :diagnostics (list (json-object "line" line "col" column "message" message))
                           :repairs (list (repair "check" "Locate the syntax error."
                                                  (command-line context (list "check" (file-target-argument target)
                                                                              "--format" "json"))))))))))

(defun %call-with-json-file/k (ports path &key root tx lock-timeout record on-document on-error)
  "Resolve PATH, record the read when RECORD (and --tx), parse it, and call
ON-DOCUMENT (context target document)."
  (declare (type function on-document on-error))
  (call-with-inspect-file/k
   ports path :root root :tx tx :lock-timeout lock-timeout :record record :on-error on-error
   :on-file (lambda (context target)
              (%json-document/k context target
                                (lambda (document) (funcall on-document context target document))
                                on-error))))

(defun %pointer-tokens/k (pointer on-tokens on-error)
  (let ((tokens (parse-json-pointer pointer)))
    (if (eq tokens :invalid)
        (fail on-error "argument.invalid"
              (format nil "~S is not a JSON pointer (\"\" or /a/b, with ~~0 for ~~ and ~~1 for /)" pointer)
              :repairs (list (repair "whole-document" "Start from the document's top-level keys."
                                     "aitools schema json get")))
        (funcall on-tokens tokens))))

(defun %resolve-or-fail (context target document tokens on-found on-error)
  (resolve-json-pointer/k
   document tokens
   :on-found on-found
   :on-missing (lambda (parent-tokens parent token)
                 (let ((parent-pointer (format-json-pointer parent-tokens)))
                   (fail on-error "input.not-found"
                         (format nil "~A has no ~A (its deepest existing prefix is ~S)"
                                 (file-target-argument target) (format-json-pointer tokens) parent-pointer)
                         :candidates (mapcar (lambda (name)
                                               (json-object "pointer" (format-json-pointer (append parent-tokens (list name)))))
                                             (rank-similar token (json-child-names parent) :count 5))
                         :repairs (list (repair "list-keys" "List the keys that exist there."
                                                (command-line context (list "json" "get" (file-target-argument target)
                                                                            parent-pointer "--keys")))))))))

;;; ------------------------------------------------------------ json get

(defun %max-bytes/k (text on-bytes on-error)
  (let ((bytes (handler-case (size-bytes (parse-size text)) (error () nil))))
    (if (and bytes (plusp bytes))
        (funcall on-bytes bytes)
        (fail on-error "argument.invalid" (format nil "--max-bytes ~S is not a size (<n>, <n>KiB, <n>MiB)" text)
              :repairs (list (repair "default-size" "Use the default limit." "aitools schema json get"))))))

(defun %json-get-fields (context target pointer value keys raw max-bytes)
  "(VALUES fields truncated) for `json get`."
  (let* ((head (list (cons "path" (file-target-argument target))
                     (cons "pointer" pointer)
                     (cons "type" (json-type-name value))
                     (cons "length" (json-or-null (json-value-length value)))))
         (body (cond (keys (list (cons "keys" (json-child-names value))))
                     (raw (list (cons "text" (if (stringp value) value (render-json value)))))
                     (t (list (cons "value" value)))))
         (rendered (render-json (cdr (first body)))))
    (if (<= (%utf8-length rendered) max-bytes)
        (values (append head body
                        (list (cons "truncated" (json-false))
                              (cons "approx_tokens" (approx-token-count (length rendered)))))
                nil)
        (let ((preview (subseq rendered 0 (min (length rendered) max-bytes))))
          (values (append head
                          (list (cons "value_preview" preview)
                                (cons "truncated" t)
                                (cons "approx_tokens" (approx-token-count (length preview)))
                                (cons "next_commands"
                                      (remove nil
                                              (list (and (not keys) (member (json-type-name value) '("object" "array") :test #'string=)
                                                         (command-line context (list "json" "get" (file-target-argument target)
                                                                                     pointer "--keys")))
                                                    (and (json-array-value-p value) (plusp (length value))
                                                         (command-line context (list "json" "get" (file-target-argument target)
                                                                                     (concatenate 'string pointer "/0")))))))))
                  t)))))

(defun json-get-flow (ports path pointer &key root tx lock-timeout (max-bytes "16KiB") keys raw
                                            on-ok on-partial on-error)
  "`json get`. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (%max-bytes/k
   max-bytes
   (lambda (limit)
     (%pointer-tokens/k
      pointer
      (lambda (tokens)
        (%call-with-json-file/k
         ports path :root root :tx tx :lock-timeout lock-timeout :record t :on-error on-error
         :on-document
         (lambda (context target document)
           (%resolve-or-fail
            context target document tokens
            (lambda (value)
              (if (and keys (not (member (json-type-name value) '("object" "array") :test #'string=)))
                  (fail on-error "argument.invalid"
                        (format nil "--keys needs an object or array; ~A is a ~A" pointer (json-type-name value))
                        :repairs (list (repair "get-value" "Get the value itself."
                                               (command-line context (list "json" "get" path pointer)))))
                  (multiple-value-bind (fields truncated) (%json-get-fields context target pointer value keys raw limit)
                    (funcall (if truncated on-partial on-ok) fields))))
            on-error))))
      on-error))
   on-error))

;;; ------------------------------------------------------------ json select

(defun %where-comparisons/k (texts on-comparisons on-error)
  "COMPARISONs keyed by relative pointer tokens for each `--where` text."
  (let ((comparisons '()))
    (dolist (text texts (funcall on-comparisons (nreverse comparisons)))
      (multiple-value-bind (left operator right) (split-comparison text)
        (let ((tokens (and left (parse-json-pointer left))))
          (when (or (null left) (eq tokens :invalid))
            (return-from %where-comparisons/k
              (fail on-error "argument.invalid"
                    (format nil "--where ~S is not <rel-pointer><op><json-value> (op: = != < <= > >= ~~)" text)
                    :repairs (list (repair "where-example" "Compare a member of each element, e.g. /status=\"open\"."
                                           "aitools schema json select")))))
          (make-comparison/k tokens operator right
                             :on-comparison (lambda (comparison) (push comparison comparisons))
                             :on-error (lambda (message)
                                         (return-from %where-comparisons/k
                                           (fail on-error "input.syntax-error" message
                                                 :repairs (list (repair "fix-pattern" "Correct the regular expression."
                                                                        "aitools schema json select")))))))))))

(defun %relative-value (element tokens)
  "(VALUES value present-p) of TOKENS below ELEMENT."
  (resolve-json-pointer/k element tokens
                          :on-found (lambda (value) (values value t))
                          :on-missing (lambda (parent-tokens parent token)
                                        (declare (ignore parent-tokens parent token))
                                        (values nil nil))))

(defun %element-matches-p (element comparisons)
  (every (lambda (comparison)
           (multiple-value-bind (value present) (%relative-value element (comparison-key comparison))
             (comparison-holds-p comparison value present)))
         comparisons))

(defun %picked (element picks)
  (json-object-from-pairs
   (mapcar (lambda (pick)
             (cons pick (multiple-value-bind (value present) (%relative-value element (parse-json-pointer pick))
                          (if present value (json-null)))))
           picks)))

(defun %select-indexes (array comparisons sort-tokens desc)
  "Indexes of ARRAY's elements that satisfy COMPARISONS, sorted by the
value under SORT-TOKENS when given."
  (let ((indexes (loop for index below (length array)
                       when (%element-matches-p (aref array index) comparisons) collect index)))
    (if sort-tokens
        (flet ((key (index) (values (%relative-value (aref array index) sort-tokens))))
          (stable-sort indexes (if desc
                                   (lambda (a b) (json-value-less-p (key b) (key a)))
                                   (lambda (a b) (json-value-less-p (key a) (key b))))))
        (if desc (nreverse indexes) indexes))))

(defun %select-fields (context target pointer array indexes picks output limit)
  "(VALUES fields truncated)."
  (if (string= output "count")
      (values (list (cons "path" (file-target-argument target)) (cons "pointer" pointer)
                    (cons "mode" "count") (cons "count" (length indexes)))
              nil)
      (let* ((shown (subseq indexes 0 (min limit (length indexes))))
             (truncated (< (length shown) (length indexes))))
        (values (append (list (cons "path" (file-target-argument target)) (cons "pointer" pointer)
                              (cons "mode" "items")
                              (cons "items" (mapcar (lambda (index)
                                                      (json-object "pointer" (format nil "~A/~D" pointer index)
                                                                   "value" (if picks
                                                                               (%picked (aref array index) picks)
                                                                               (aref array index))))
                                                    shown))
                              (cons "total" (length indexes))
                              (cons "truncated" (json-bool truncated)))
                        (when truncated
                          (list (cons "next_commands"
                                      (list (command-line context (list "json" "select" (file-target-argument target) pointer
                                                                        "--output" "count")))))))
                truncated))))

(defun json-select-flow (ports path pointer &key root tx lock-timeout where pick sort-by desc (output "items")
                                               (limit 50) on-ok on-partial on-error)
  "`json select`. WHERE and PICK are lists of texts. Calls ON-OK or ON-PARTIAL
(fields), or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (let ((bad-pointer (find-if (lambda (text) (eq (parse-json-pointer text) :invalid))
                              (append pick (when sort-by (list sort-by))))))
    (if bad-pointer
        (fail on-error "argument.invalid" (format nil "~S is not a relative JSON pointer" bad-pointer)
              :repairs (list (repair "pointer-syntax" "Write members as /name or /a/b." "aitools schema json select")))
        (%pointer-tokens/k
         pointer
         (lambda (tokens)
           (%where-comparisons/k
            where
            (lambda (comparisons)
              (%call-with-json-file/k
               ports path :root root :tx tx :lock-timeout lock-timeout :record t :on-error on-error
               :on-document
               (lambda (context target document)
                 (%resolve-or-fail
                  context target document tokens
                  (lambda (array)
                    (if (not (json-array-value-p array))
                        (fail on-error "argument.invalid"
                              (format nil "~S in ~A is a ~A, not an array" pointer path (json-type-name array))
                              :repairs (list (repair "list-keys" "Find the array to select from."
                                                     (command-line context (list "json" "get" path pointer "--keys")))))
                        (let ((indexes nil) (limit-message nil))
                          (call-with-regex-limit/k
                           (lambda ()
                             (setf indexes (%select-indexes array comparisons
                                                            (and sort-by (parse-json-pointer sort-by)) desc)))
                           (lambda (message) (setf limit-message message)))
                          (if limit-message
                              (fail on-error "input.syntax-error"
                                    (format nil "a --where ~~ pattern is too complex to evaluate: ~A" limit-message)
                                    :repairs (list (repair "fix-pattern" "Simplify the regular expression."
                                                           "aitools schema json select")))
                              (multiple-value-bind (fields truncated)
                                  (%select-fields context target pointer array indexes pick output limit)
                                (funcall (if truncated on-partial on-ok) fields))))))
                  on-error))))
            on-error))
         on-error))))

;;; ------------------------------------------------------------ json diff

(defun %diff-op-json (op)
  (destructuring-bind (kind tokens old new) op
    (json-object-from-pairs
     (append (list (cons "op" (string-downcase (symbol-name kind)))
                   (cons "pointer" (format-json-pointer tokens)))
             (unless (eq kind :add) (list (cons "old" old)))
             (unless (eq kind :remove) (list (cons "new" new)))))))

(defun json-diff-flow (ports a b &key root tx lock-timeout (limit 100) on-ok on-partial on-error)
  "`json diff`. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (%call-with-json-file/k
   ports a :root root :tx tx :lock-timeout lock-timeout :on-error on-error
   :on-document
   (lambda (context target-a document-a)
     (declare (ignore context target-a))
     (%call-with-json-file/k
      ports b :root root :tx tx :lock-timeout lock-timeout :on-error on-error
      :on-document
      (lambda (context target-b document-b)
        (declare (ignore context target-b))
        (let* ((ops (json-diff-ops document-a document-b))
               (shown (subseq ops 0 (min limit (length ops))))
               (truncated (< (length shown) (length ops))))
          (funcall (if truncated on-partial on-ok)
                   (list (cons "a" a) (cons "b" b)
                         (cons "identical" (json-bool (null ops)))
                         (cons "ops" (mapcar #'%diff-op-json shown))
                         (cons "total" (length ops))
                         (cons "truncated" (json-bool truncated))))))))))
