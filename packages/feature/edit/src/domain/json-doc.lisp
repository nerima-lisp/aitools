;;;; packages/feature/edit/src/domain/json-doc.lisp
;;;;
;;;; The JSON write commands: a value model that keeps
;;;; object key order and each number's source text, RFC 6901 pointers,
;;;; `set`/`delete`, RFC 7386 merge patch, RFC 6902 patch, and a serializer
;;;; that re-indents with the width detected in the original file.
;;;;
;;;; Values: JSON-OBJ (ordered members), SIMPLE-VECTOR (array), STRING,
;;;; JSON-NUM (source text), T, and json-kit's +JSON-FALSE+ / +JSON-NULL+.
;;;; Every operation is non-destructive: the patch commands must leave the
;;;; file untouched when a later operation fails, and building new
;;;; values keeps that trivially true.
(in-package #:aitools.edit.domain)

(defstruct (json-obj (:constructor make-json-obj (members)) (:copier nil))
  ;; ((key . value) ...) in document order
  (members '() :type list :read-only t))

(defstruct (json-num (:constructor make-json-num (text)) (:copier nil))
  (text "" :type string :read-only t))

(defun parse-json-text (text)
  "TEXT parsed into the edit value model; signals JSON-KIT:JSON-PARSE-ERROR."
  (json-kit:parse text
                  :object-type :alist
                  :duplicate-key-policy :last
                  :object-hook #'make-json-obj
                  :number-decoder (lambda (token &rest flags) (declare (ignore flags)) (make-json-num token))))

;;; ------------------------------------------------------------ serializing

(defun %write-json-string (string out)
  (write-string (json-kit:stringify string) out))

(defun serialize-json (value &key indent sort-keys)
  "VALUE as JSON text. INDENT is the string one level adds (\"  \", a tab)
or NIL for the minified form. SORT-KEYS orders object members by key."
  (with-output-to-string (out)
    (labels ((newline (level)
               (when indent
                 (write-char #\Newline out)
                 (dotimes (i level) (write-string indent out))))
             (emit (value level)
               (cond
                 ((json-obj-p value)
                  (let ((members (json-obj-members value)))
                    (when sort-keys (setf members (sort (copy-list members) #'string< :key #'car)))
                    (if (null members)
                        (write-string "{}" out)
                        (progn
                          (write-char #\{ out)
                          (loop for (member . rest) on members
                                do (newline (1+ level))
                                   (%write-json-string (car member) out)
                                   (write-string (if indent ": " ":") out)
                                   (emit (cdr member) (1+ level))
                                   (when rest (write-char #\, out)))
                          (newline level)
                          (write-char #\} out)))))
                 ((and (vectorp value) (not (stringp value)))
                  (if (zerop (length value))
                      (write-string "[]" out)
                      (progn
                        (write-char #\[ out)
                        (loop for index from 0 below (length value)
                              do (newline (1+ level))
                                 (emit (aref value index) (1+ level))
                                 (when (< (1+ index) (length value)) (write-char #\, out)))
                        (newline level)
                        (write-char #\] out))))
                 ((stringp value) (%write-json-string value out))
                 ((json-num-p value) (write-string (json-num-text value) out))
                 ((eq value t) (write-string "true" out))
                 ((json-kit:json-false-p value) (write-string "false" out))
                 ((json-kit:json-null-p value) (write-string "null" out))
                 (t (refuse "argument.invalid" "cannot serialize ~S" value)))))
      (emit value 0))))

(defun detect-json-indent (text)
  "The indentation unit TEXT uses: the leading whitespace of its first
indented line, or NIL when the value is written on one line."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) text)))
    (when (find #\Newline trimmed)
      (loop with start = 0
            for newline = (position #\Newline trimmed :start start)
            while newline
            do (let* ((line-start (1+ newline))
                      (end (position-if-not (lambda (c) (member c '(#\Space #\Tab))) trimmed :start line-start)))
                 (when (and (> end line-start) (not (member (char trimmed end) '(#\Newline #\Return))))
                   (return (subseq trimmed line-start end)))
                 (setf start line-start))
            finally (return "  ")))))

;;; ---------------------------------------------------------------- pointers

(defun parse-json-pointer (pointer)
  "RFC 6901 POINTER as a list of reference tokens; signals JSON-EDIT-ERROR
(argument.invalid) for a malformed pointer. Delegates to the kernel's shared
pointer module, which the read side (`json get`/`query`/`diff`) uses too;
its :INVALID becomes the edit refusal."
  (let ((tokens (aitools.kernel.domain:parse-json-pointer pointer)))
    (if (eq tokens :invalid)
        (refuse "argument.invalid" "JSON pointer ~S must be empty or start with /, with valid ~~ escapes" pointer)
        tokens)))

(defun format-json-pointer (tokens)
  (aitools.kernel.domain:format-json-pointer tokens))

(defun %array-index (token length &key allow-end)
  "TOKEN as an index into an array of LENGTH, or NIL (kernel's shared rule)."
  (aitools.kernel.domain:json-pointer-array-index token length :allow-end allow-end))

(defun %not-found (tokens)
  (refuse "input.not-found" "no value at JSON pointer ~S" (format-json-pointer tokens)))

(defun json-pointer-get (value tokens)
  (let ((current value))
    (loop for (token . rest) on tokens
          for seen = (list token) then (append seen (list token))
          do (setf current
                   (cond
                     ((json-obj-p current)
                      (let ((member (assoc token (json-obj-members current) :test #'string=)))
                        (if member (cdr member) (%not-found seen))))
                     ((and (vectorp current) (not (stringp current)))
                      (let ((index (%array-index token (length current))))
                        (if index (aref current index) (%not-found seen))))
                     (t (%not-found seen)))))
    current))

(defun %update-at (value tokens function)
  "VALUE with the container addressed by (BUTLAST TOKENS) replaced by
(FUNCTION container last-token). Missing parents are INPUT.NOT-FOUND."
  (if (null (rest tokens))
      (funcall function value (first tokens))
      (let ((token (first tokens)))
        (cond
          ((json-obj-p value)
           (let ((member (assoc token (json-obj-members value) :test #'string=)))
             (unless member (%not-found (list token)))
             (make-json-obj (mapcar (lambda (pair)
                                      (if (eq pair member)
                                          (cons (car pair) (%update-at (cdr pair) (rest tokens) function))
                                          pair))
                                    (json-obj-members value)))))
          ((and (vectorp value) (not (stringp value)))
           (let ((index (%array-index token (length value))))
             (unless index (%not-found (list token)))
             (let ((copy (copy-seq value)))
               (setf (aref copy index) (%update-at (aref value index) (rest tokens) function))
               copy)))
          (t (%not-found (list token)))))))

(defun %object-put (object key new)
  (if (assoc key (json-obj-members object) :test #'string=)
      (make-json-obj (mapcar (lambda (pair) (if (string= (car pair) key) (cons key new) pair))
                             (json-obj-members object)))
      (make-json-obj (append (json-obj-members object) (list (cons key new))))))

(defun json-add (value tokens new &key (array-mode :insert))
  "VALUE with NEW placed at TOKENS: an object member is added or replaced;
an array element is inserted (RFC 6902 `add`) or, with ARRAY-MODE :REPLACE,
replaced unless the token is `-` (`json set`). The root pointer replaces
the whole value."
  (if (null tokens)
      new
      (%update-at value tokens
                  (lambda (container token)
                    (cond
                      ((json-obj-p container) (%object-put container token new))
                      ((and (vectorp container) (not (stringp container)))
                       (let ((index (%array-index token (length container) :allow-end t)))
                         (cond
                           ((null index) (%not-found (list token)))
                           ((and (eq array-mode :replace) (< index (length container)) (string/= token "-"))
                            (let ((copy (copy-seq container))) (setf (aref copy index) new) copy))
                           (t (concatenate 'simple-vector (subseq container 0 index) (vector new)
                                           (subseq container index))))))
                      (t (%not-found (list token))))))))

(defun json-remove (value tokens)
  (when (null tokens)
    (refuse "argument.invalid" "the root value cannot be removed"))
  (%update-at value tokens
              (lambda (container token)
                (cond
                  ((json-obj-p container)
                   (unless (assoc token (json-obj-members container) :test #'string=)
                     (%not-found (list token)))
                   (make-json-obj (remove token (json-obj-members container) :key #'car :test #'string=)))
                  ((and (vectorp container) (not (stringp container)))
                   (let ((index (%array-index token (length container))))
                     (unless index (%not-found (list token)))
                     (concatenate 'simple-vector (subseq container 0 index) (subseq container (1+ index)))))
                  (t (%not-found (list token)))))))

(defun json-replace (value tokens new)
  (json-pointer-get value tokens)
  (if (null tokens)
      new
      (%update-at value tokens
                  (lambda (container token)
                    (if (json-obj-p container)
                        (%object-put container token new)
                        (let ((copy (copy-seq container)))
                          (setf (aref copy (%array-index token (length container))) new)
                          copy))))))

;;; ------------------------------------------------------------- equality

(defun %classify-json (value)
  "Map an edit value-model VALUE to the (VALUES KIND PAYLOAD) that
AITOOLS.KERNEL.DOMAIN:JSON-EQUAL expects."
  (cond
    ((json-obj-p value) (values :object (json-obj-members value)))
    ((and (vectorp value) (not (stringp value))) (values :array value))
    ((json-num-p value) (values :number (json-num-text value)))
    ((stringp value) (values :string value))
    ((eq value t) :true)
    ((json-kit:json-false-p value) :false)
    ;; The value model's one value left: json-kit's +JSON-NULL+.
    (t :null)))

(defun json-equal (a b)
  "RFC 6902 `test` equality via the kernel's shared JSON-EQUAL: numbers
by IEEE double value (RFC 8259), objects regardless of member order, arrays
element-wise. The read side (`json get`/`diff`) uses the same rule."
  (aitools.kernel.domain:json-equal a b #'%classify-json))

;;; ------------------------------------------------------------- patches

(defun json-merge-patch (target patch)
  "RFC 7386: members of an object PATCH are merged recursively, `null`
removes; any other PATCH replaces TARGET. Existing members keep their
position and new ones are appended in PATCH's order."
  (if (not (json-obj-p patch))
      patch
      (let ((result (if (json-obj-p target) target (make-json-obj '()))))
        (dolist (pair (json-obj-members patch) result)
          (let ((key (car pair)) (value (cdr pair)))
            (setf result
                  (if (json-kit:json-null-p value)
                      (make-json-obj (remove key (json-obj-members result) :key #'car :test #'string=))
                      (let ((existing (assoc key (json-obj-members result) :test #'string=)))
                        (%object-put result key (json-merge-patch (and existing (cdr existing)) value))))))))))

(defun %op-field (operation name &key required)
  (let ((member (assoc name (json-obj-members operation) :test #'string=)))
    (when (and required (null member))
      (refuse "argument.invalid" "JSON Patch operation lacks ~S" name))
    (cdr member)))

(defun %op-pointer (operation name)
  (let ((text (%op-field operation name :required t)))
    (unless (stringp text)
      (refuse "argument.invalid" "JSON Patch member ~S must be a string" name))
    (parse-json-pointer text)))

(defun json-apply-patch (value operations)
  "RFC 6902: apply OPERATIONS (a JSON array of operation objects) in order.
A failing `test` signals JSON-EDIT-ERROR selection.no-match; a missing path
input.not-found; a malformed operation argument.invalid."
  (unless (and (vectorp operations) (not (stringp operations)))
    (refuse "argument.invalid" "a JSON Patch document must be an array"))
  (loop for operation across operations
        for index from 0
        do (unless (json-obj-p operation)
             (refuse "argument.invalid" "JSON Patch operation ~D is not an object" index))
           (let ((op (%op-field operation "op" :required t)))
             (setf value
                   (cond
                     ((equal op "add")
                      (json-add value (%op-pointer operation "path") (%op-field operation "value" :required t)))
                     ((equal op "remove") (json-remove value (%op-pointer operation "path")))
                     ((equal op "replace")
                      (json-replace value (%op-pointer operation "path") (%op-field operation "value" :required t)))
                     ((equal op "move")
                      (let ((from (%op-pointer operation "from")) (path (%op-pointer operation "path")))
                        (cond
                          ((equal from path) value)
                          ((and (< (length from) (length path)) (equal from (subseq path 0 (length from))))
                           (refuse "argument.invalid" "JSON Patch cannot move a value into itself"))
                          (t (let ((moved (json-pointer-get value from)))
                               (json-add (json-remove value from) path moved))))))
                     ((equal op "copy")
                      (json-add value (%op-pointer operation "path")
                                (json-pointer-get value (%op-pointer operation "from"))))
                     ((equal op "test")
                      (let ((path (%op-pointer operation "path")))
                        (unless (json-equal (json-pointer-get value path) (%op-field operation "value" :required t))
                          (refuse "selection.no-match" "JSON Patch test failed at ~S (operation ~D)"
                                      (format-json-pointer path) index))
                        value))
                     (t (refuse "argument.invalid" "unknown JSON Patch op ~S" op))))))
  value)

(defun parse-json-text/k (text &key on-value on-invalid)
  "TEXT parsed (see PARSE-JSON-TEXT): ON-VALUE (value) or ON-INVALID
(message)."
  (declare (type function on-value on-invalid))
  (let ((value (handler-case (parse-json-text text)
                 (json-kit:json-kit-error (condition)
                   (return-from parse-json-text/k (funcall on-invalid (princ-to-string condition)))))))
    (funcall on-value value)))

(defun json-string-literal (string)
  "STRING written as a JSON string."
  (json-kit:stringify string))
