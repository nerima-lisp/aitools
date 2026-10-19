;;;; packages/feature/inspect/src/domain/json-query.lisp
;;;;
;;;; The read side of the `json` group (`get`, `select`, `diff`): RFC 6901
;;;; pointers, `--where` conditions with the shared comparison operators
;;;; (= != < <= > >= ~), `--sort-by` ordering, and the structural diff of
;;;; `json diff`, which ignores key order and whitespace.
(in-package #:aitools.inspect.domain)

;;; ------------------------------------------------------------ pointers

;;; RFC 6901 pointers and array-index resolution live in
;;; AITOOLS.KERNEL.DOMAIN (one canonical implementation shared with the
;;; write side). These forward to it under the names the inspect domain and
;;; its tests already use.

(defun parse-json-pointer (text)
  "The reference tokens of pointer TEXT (\"\" is the whole document), or
:INVALID when TEXT is not an RFC 6901 pointer."
  (aitools.kernel.domain:parse-json-pointer text))

(defun format-json-pointer (tokens)
  (aitools.kernel.domain:format-json-pointer tokens))

(defparameter *json-value-model*
  (aitools.kernel.domain:make-json-value-model
   :object-p #'json-object-value-p
   :object-members #'json-object-pairs
   :object-from-members #'json-object-from-pairs
   :array-p #'json-array-value-p
   :array-elements #'identity
   :array-from-elements (lambda (elements) (coerce elements 'simple-vector))
   :array-insert (lambda (array index value)
                   (concatenate 'vector (subseq array 0 index) (vector value) (subseq array index)))
   :array-remove (lambda (array index)
                   (concatenate 'vector (subseq array 0 index) (subseq array (1+ index))))
   :null-p #'json-null-value-p
   :string-p #'stringp
   :string-text #'identity
   :string-length #'length
   :equal (lambda (a b) (aitools.kernel.domain:json-equal a b #'%classify-json))))

(defun json-child (value token)
  "(VALUES child present-p) of VALUE under one reference TOKEN."
  (aitools.kernel.domain:json-model-child *json-value-model* value token))

(aitools.kernel.domain:define-flow
 (resolve-json-pointer/k document tokens &key on-found on-missing)
 "Walk TOKENS from DOCUMENT and continue with ON-FOUND (value) or ON-MISSING
 (parent-tokens parent token), PARENT being the deepest value that exists."
 (let ((value document)
       (missing nil))
   (loop for token in tokens
         for depth from 0
         while (null missing)
         do (multiple-value-bind (child present) (json-child value token)
              (if present
                  (setf value child)
                  (setf missing
                        (list (subseq tokens 0 depth) value token)))))
   (if missing
       (aitools.kernel.domain:flow-continue
        :on-missing
        (first missing)
        (second missing)
        (third missing))
       (aitools.kernel.domain:flow-continue :on-found value))))

(defun json-child-names (value)
  "The keys of an object, or the indexes (as strings) of an array."
  (aitools.kernel.domain:json-model-child-names *json-value-model* value))

(defun json-value-length (value)
  "Elements of an array, keys of an object, characters of a string, else NIL."
  (aitools.kernel.domain:json-model-value-length *json-value-model* value))

;;; ------------------------------------------------------------ comparison

(progn
 (defparameter *comparison-operators* '("!=" "<=" ">=" "=" "<" ">" "~")
   "The `--where` comparison operators, two-character ones first so `<=` is not
read as `<`.")
 (defparameter *comparison-position-before-form*
   '(lambda (position best)
      (or (null best) (< position best))))
 (defparameter *comparison-position-before-p*
   (compile nil *comparison-position-before-form*)))

(defun split-comparison (text)
 "(VALUES left operator right) splitting TEXT at the earliest operator
(the longest one at that position), or NIL when TEXT has none."
 (let ((best nil) (best-operator nil))
   (dolist (operator *comparison-operators*)
     (let ((position (search operator text)))
       (when (and position
                  (funcall *comparison-position-before-p* position best))
         (setf best position best-operator operator))))
   (when best
     (values (subseq text 0 best)
             best-operator
             (subseq text (+ best (length best-operator)))))))

(aitools.kernel.domain:define-flow
 (parse-comparison-value text &key on-value)
 "TEXT as a JSON value when it parses as one, else as a plain string, so
`--where /name=alice` works without JSON quotes."
 (aitools.kernel.domain:flow-call
  (parse-json-document/k text)
  (:on-value (value)
    (aitools.kernel.domain:flow-continue :on-value value))
  (:on-error (message line column)
    (declare (ignore message line column))
    (aitools.kernel.domain:flow-continue :on-value text))))

(defstruct (comparison (:constructor %make-comparison (key operator value regex)) (:copier nil))
  "KEY is what the left side names (pointer tokens or a column name);
OPERATOR a keyword; VALUE the right side; REGEX the compiled `~` pattern."
  (key nil :read-only t)
  (operator :equal :read-only t)
  (value nil :read-only t)
  (regex nil :read-only t))

(defun %operator-keyword (operator)
  (cdr (assoc operator '(("=" . :equal) ("!=" . :not-equal) ("<" . :less) ("<=" . :less-equal)
                         (">" . :greater) (">=" . :greater-equal) ("~" . :match))
              :test #'string=)))

(aitools.kernel.domain:define-flow
 (make-comparison/k key operator right &key on-comparison on-error)
 "A COMPARISON of KEY OPERATOR RIGHT (the texts from SPLIT-COMPARISON);
ON-ERROR (message) for a bad `~` pattern."
 (declare (type function on-comparison on-error))
 (let ((keyword (%operator-keyword operator)))
   (if (eq keyword :match)
       (aitools.kernel.domain:flow-call
        (compile-pattern/k right)
        (:on-compiled (regex)
          (aitools.kernel.domain:flow-continue
           :on-comparison
           (%make-comparison key keyword right regex)))
        (:on-error (message)
          (aitools.kernel.domain:flow-continue :on-error message)))
       (aitools.kernel.domain:flow-call
        (parse-comparison-value right)
        (:on-value (value)
          (aitools.kernel.domain:flow-continue
           :on-comparison
           (%make-comparison key keyword value nil)))))))

(defun %ordered-compare (left right)
  "-1, 0, or 1 for two numbers or two strings, else NIL."
  (cond ((and (numberp left) (numberp right)) (cond ((< left right) -1) ((> left right) 1) (t 0)))
        ((and (stringp left) (stringp right)) (cond ((string< left right) -1) ((string> left right) 1) (t 0)))
        (t nil)))

(defun comparison-holds-p (comparison value present)
  "Whether VALUE (PRESENT false when the left side does not exist) satisfies
COMPARISON. A missing left side satisfies only `!=`; ordering operators
hold only between two numbers or two strings; `~` matches strings."
  (let ((right (comparison-value comparison)))
    (if (not present)
        (eq (comparison-operator comparison) :not-equal)
        (ecase (comparison-operator comparison)
          (:equal (json-equal value right))
          (:not-equal (not (json-equal value right)))
          (:match (and (stringp value) (pattern-matches-p (comparison-regex comparison) value)))
          ((:less :less-equal :greater :greater-equal)
           (let ((order (%ordered-compare value right)))
             (and order
                  (ecase (comparison-operator comparison)
                    (:less (< order 0))
                    (:less-equal (<= order 0))
                    (:greater (> order 0))
                    (:greater-equal (>= order 0))))))))))

(defun %sort-rank (value)
  (cond ((or (null value) (json-null-value-p value)) 0)
        ((or (eq value t) (json-false-value-p value)) 1)
        ((numberp value) 2)
        ((stringp value) 3)
        (t 4)))

(defun json-value-less-p (a b)
  "Sort order for `--sort-by` and `--sort`: missing and null, then
booleans (false first), numbers, strings, and containers last."
  (let ((rank-a (%sort-rank a)) (rank-b (%sort-rank b)))
    (cond ((/= rank-a rank-b) (< rank-a rank-b))
          ((= rank-a 1) (and (json-false-value-p a) (eq b t)))
          ((= rank-a 2) (< a b))
          ((= rank-a 3) (string< a b))
          (t nil))))

;;; ------------------------------------------------------------ diff

(defun json-diff-ops (a b &optional (path '()))
  "RFC 6902 style changes turning A into B, as a list of (op tokens old new)
with OP :ADD, :REMOVE, or :REPLACE, in document order. Object key order
and number spelling (1 vs 1.0) are not differences."
  (aitools.kernel.domain:json-model-diff-ops *json-value-model* a b path))
