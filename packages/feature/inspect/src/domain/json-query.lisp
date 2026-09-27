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

(defun json-child (value token)
  "(VALUES child present-p) of VALUE under one reference TOKEN."
  (cond ((json-object-value-p value) (json-object-get value token))
        ((json-array-value-p value)
         (let ((index (aitools.kernel.domain:json-pointer-array-index token (length value))))
           (if index (values (aref value index) t) (values nil nil))))
        (t (values nil nil))))

(defun resolve-json-pointer/k (document tokens &key on-found on-missing)
  "Walk TOKENS from DOCUMENT and call ON-FOUND (value) or ON-MISSING
(parent-tokens parent token), PARENT being the deepest value that exists."
  (declare (type function on-found on-missing))
  (let ((value document))
    (loop for (token . rest) on tokens
          for depth from 0
          do (multiple-value-bind (child present) (json-child value token)
               (unless present
                 (return-from resolve-json-pointer/k
                   (funcall on-missing (subseq tokens 0 depth) value token)))
               (setf value child)))
    (funcall on-found value)))

(defun json-child-names (value)
  "The keys of an object, or the indexes (as strings) of an array."
  (cond ((json-object-value-p value) (mapcar #'car (json-object-pairs value)))
        ((json-array-value-p value) (loop for index below (length value) collect (princ-to-string index)))
        (t '())))

(defun json-value-length (value)
  "Elements of an array, keys of an object, characters of a string, else NIL."
  (cond ((json-object-value-p value) (length (json-object-pairs value)))
        ((json-array-value-p value) (length value))
        ((stringp value) (length value))
        (t nil)))

;;; ------------------------------------------------------------ comparison

(defparameter *comparison-operators* '("!=" "<=" ">=" "=" "<" ">" "~")
  "The `--where` comparison operators, two-character ones first so `<=` is not
read as `<`.")

(defun split-comparison (text)
  "(VALUES left operator right) splitting TEXT at the earliest operator
(the longest one at that position), or NIL when TEXT has none."
  (let ((best nil) (best-operator nil))
    (dolist (operator *comparison-operators*)
      (let ((position (search operator text)))
        (when (and position (or (null best) (< position best)))
          (setf best position best-operator operator))))
    (when best
      (values (subseq text 0 best) best-operator (subseq text (+ best (length best-operator)))))))

(defun parse-comparison-value (text)
  "TEXT as a JSON value when it parses as one, else as a plain string, so
`--where /name=alice` works without JSON quotes."
  (parse-json-document/k text :on-value #'identity
                              :on-error (lambda (message line column)
                                          (declare (ignore message line column))
                                          text)))

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

(defun make-comparison/k (key operator right &key on-comparison on-error)
  "A COMPARISON of KEY OPERATOR RIGHT (the texts from SPLIT-COMPARISON);
ON-ERROR (message) for a bad `~` pattern."
  (declare (type function on-comparison on-error))
  (let ((keyword (%operator-keyword operator)))
    (if (eq keyword :match)
        (compile-pattern/k right :on-error on-error
                                 :on-compiled (lambda (regex)
                                                (funcall on-comparison (%make-comparison key keyword right regex))))
        (funcall on-comparison (%make-comparison key keyword (parse-comparison-value right) nil)))))

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
  (cond
    ((and (json-object-value-p a) (json-object-value-p b))
     (let ((keys-a (mapcar #'car (json-object-pairs a)))
           (keys-b (mapcar #'car (json-object-pairs b))))
       (append
        (loop for key in keys-a
              for (value-b present) = (multiple-value-list (json-object-get b key))
              append (if present
                         (json-diff-ops (json-object-get a key) value-b (append path (list key)))
                         (list (list :remove (append path (list key)) (json-object-get a key) nil))))
        (loop for key in keys-b
              unless (member key keys-a :test #'string=)
                collect (list :add (append path (list key)) nil (json-object-get b key))))))
    ((and (json-array-value-p a) (json-array-value-p b))
     (append
      (loop for index below (min (length a) (length b))
            append (json-diff-ops (aref a index) (aref b index) (append path (list (princ-to-string index)))))
      (loop for index from (1- (length a)) downto (length b)
            collect (list :remove (append path (list (princ-to-string index))) (aref a index) nil))
      (loop for index from (length a) below (length b)
            collect (list :add (append path (list (princ-to-string index))) nil (aref b index)))))
    ((json-equal a b) '())
    (t (list (list :replace path a b)))))
