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

(defparameter *json-value-model*
  (aitools.kernel.domain:make-json-value-model
   :object-p #'json-obj-p
   :object-members #'json-obj-members
   :object-from-members #'make-json-obj
   :array-p (lambda (value) (and (vectorp value) (not (stringp value))))
   :array-elements #'identity
   :array-from-elements (lambda (elements) (coerce elements 'simple-vector))
   :array-insert (lambda (array index value)
                   (concatenate 'vector (subseq array 0 index) (vector value) (subseq array index)))
   :array-remove (lambda (array index)
                   (concatenate 'vector (subseq array 0 index) (subseq array (1+ index))))
   :null-p #'json-kit:json-null-p
   :string-p #'stringp
   :string-text #'identity
   :string-length #'length
   :equal (lambda (a b) (aitools.kernel.domain:json-equal a b #'%classify-json))))

(defun %with-json-model-errors (function)
  (handler-case (funcall function)
    (aitools.kernel.domain:json-model-error (condition)
      (refuse (aitools.kernel.domain:json-model-error-code condition)
              "~A"
              (aitools.kernel.domain:json-model-error-message condition)))))

(defun json-pointer-get (value tokens)
  (%with-json-model-errors
   (lambda () (aitools.kernel.domain:json-model-pointer-get *json-value-model* value tokens))))

(defun json-add (value tokens new &key (array-mode :insert))
  (%with-json-model-errors
   (lambda ()
     (aitools.kernel.domain:json-model-add *json-value-model* value tokens new
                                            :array-mode array-mode))))

(defun json-remove (value tokens)
  (%with-json-model-errors
   (lambda () (aitools.kernel.domain:json-model-remove *json-value-model* value tokens))))

(defun json-replace (value tokens new)
  (%with-json-model-errors
   (lambda () (aitools.kernel.domain:json-model-replace *json-value-model* value tokens new))))

(defun json-equal (a b)
  "RFC 6902 `test` equality via the kernel's shared JSON-EQUAL: numbers
by IEEE double value (RFC 8259), objects regardless of member order, arrays
element-wise. The read side (`json get`/`diff`) uses the same rule."
  (aitools.kernel.domain:json-model-equal *json-value-model* a b))

(defun json-merge-patch (target patch)
  (%with-json-model-errors
   (lambda () (aitools.kernel.domain:json-model-merge-patch *json-value-model* target patch))))

(defun json-apply-patch (value operations)
  (%with-json-model-errors
   (lambda () (aitools.kernel.domain:json-model-apply-patch *json-value-model* value operations))))

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
