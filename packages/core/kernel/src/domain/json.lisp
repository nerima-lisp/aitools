;;;; packages/core/kernel/src/domain/json.lisp
;;;;
;;;; RFC 6901 JSON pointers and JSON value equality, shared by the read side
;;;; (`json get`/`query`/`diff`, which holds json-kit values with numbers
;;;; already parsed) and the write side (`json set`/`patch`, which keeps each
;;;; number's source text so an untouched number is written back unchanged).
;;;; Kernel names no JSON library, so JSON-EQUAL reaches either
;;;; representation through a classifier function its caller supplies.
(in-package #:aitools.kernel.domain)

;;; ------------------------------------------------------------ pointers

(defun %unescape-pointer-token (token)
  "TOKEN with `~1` -> `/` then `~0` -> `~` (RFC 6901 section 4), or NIL when
a `~` is followed by anything else."
  (with-output-to-string (out)
    (loop with index = 0
          while (< index (length token))
          do (let ((char (char token index)))
               (if (char= char #\~)
                   (let ((next (and (< (1+ index) (length token)) (char token (1+ index)))))
                     (case next
                       (#\0 (write-char #\~ out))
                       (#\1 (write-char #\/ out))
                       (t (return-from %unescape-pointer-token nil)))
                     (incf index 2))
                   (progn (write-char char out) (incf index)))))))

(defun parse-json-pointer (text)
  "The reference tokens of pointer TEXT (\"\" is the whole document, NIL),
or :INVALID when TEXT is not an RFC 6901 pointer."
  (cond ((zerop (length text)) '())
        ((char/= (char text 0) #\/) :invalid)
        (t (let ((tokens (loop with start = 1
                               for slash = (position #\/ text :start start)
                               collect (%unescape-pointer-token (subseq text start (or slash (length text))))
                               while slash
                               do (setf start (1+ slash)))))
             (if (member nil tokens) :invalid tokens)))))

(defun format-json-pointer (tokens)
  (with-output-to-string (out)
    (dolist (token tokens)
      (write-char #\/ out)
      (loop for char across token
            do (case char
                 (#\~ (write-string "~0" out))
                 (#\/ (write-string "~1" out))
                 (t (write-char char out)))))))

(defun json-pointer-array-index (token length &key allow-end)
  "The index TOKEN names in an array of LENGTH, or NIL: ASCII digits with no
leading zero, below LENGTH. ALLOW-END also accepts `-` and LENGTH itself, the
position after the last element that RFC 6902 `add` appends at."
  (cond ((string= token "-") (and allow-end length))
        ((and (plusp (length token))
              (every (lambda (char) (char<= #\0 char #\9)) token)
              (or (= (length token) 1) (char/= (char token 0) #\0)))
         (let ((index (parse-integer token)))
           (and (if allow-end (<= index length) (< index length)) index)))
        (t nil)))

;;; ------------------------------------------------------------ equality

(defun %rational-double (value)
  (if (> (abs value) (rational most-positive-double-float))
      (if (minusp value) :-infinity :+infinity)
      (coerce value 'double-float)))

(defun %number-text-double (text)
  "The double nearest to the JSON number TEXT, or :+INFINITY / :-INFINITY
past the double range. The decimal exponent is bounded before any
arithmetic, so `1e999999999` costs no more than `1e9`."
  (let* ((negative (char= (char text 0) #\-))
         (start (if negative 1 0))
         (exponent-at (position-if (lambda (char) (char-equal char #\e)) text))
         (mantissa-end (or exponent-at (length text)))
         (dot (position #\. text :start start :end mantissa-end))
         (digits (string-left-trim "0" (remove #\. (subseq text start mantissa-end))))
         (exponent (- (if exponent-at (parse-integer text :start (1+ exponent-at)) 0)
                      (if dot (- mantissa-end dot 1) 0)))
         ;; The value lies in [10^(MAGNITUDE-1), 10^MAGNITUDE).
         (magnitude (+ (length digits) exponent)))
    (cond ((zerop (length digits)) 0d0)
          ((> magnitude 309) (if negative :-infinity :+infinity))
          ((<= magnitude -324) 0d0)
          (t (%rational-double (* (if negative -1 1) (parse-integer digits) (expt 10 exponent)))))))

(defun %json-number-double (number)
  (etypecase number
    (float (coerce number 'double-float))
    (rational (%rational-double number))
    (string (%number-text-double number))))

(defun %last-wins-members (members)
  "MEMBERS with a repeated key collapsed to its last value."
  (let ((result '()))
    (dolist (member members result)
      (let ((existing (assoc (car member) result :test #'string=)))
        (if existing
            (setf (cdr existing) (cdr member))
            (push (cons (car member) (cdr member)) result))))))

(defun json-equal (a b classify)
  "True when JSON values A and B are equal: numbers when they parse to the
same IEEE double (RFC 8259 section 6's interoperable reading, so
1.00000000000000001 equals 1), strings by code points, arrays element by
element, objects member by member regardless of order with a repeated key's
last value winning, and anything else when CLASSIFY gives both the same kind.
CLASSIFY maps a value to (VALUES KIND PAYLOAD): :OBJECT with an alist of (KEY
. VALUE) members, :ARRAY with a sequence of values, :NUMBER with a real or
the number's JSON text, :STRING with the string, or another keyword naming a
literal (:TRUE, :FALSE, :NULL) with any payload."
  (declare (type function classify))
  (multiple-value-bind (kind-a payload-a) (funcall classify a)
    (multiple-value-bind (kind-b payload-b) (funcall classify b)
      (and (eq kind-a kind-b)
           (case kind-a
             (:number (let ((x (%json-number-double payload-a)) (y (%json-number-double payload-b)))
                        (if (and (floatp x) (floatp y)) (= x y) (eq x y))))
             (:string (string= payload-a payload-b))
             (:array (and (= (length payload-a) (length payload-b))
                          (every (lambda (x y) (json-equal x y classify)) payload-a payload-b)))
             (:object (let ((members-a (%last-wins-members payload-a))
                            (members-b (%last-wins-members payload-b)))
                        (and (= (length members-a) (length members-b))
                             (every (lambda (member)
                                      (let ((other (assoc (car member) members-b :test #'string=)))
                                        (and other (json-equal (cdr member) (cdr other) classify))))
                                    members-a))))
             (t t))))))

;;; ------------------------------------------------ scalar field formatters

(defun iso8601-utc (universal-time)
  "UNIVERSAL-TIME (CL universal time) as RFC 3339 UTC to the second,
`YYYY-MM-DDTHH:MM:SSZ`. The one timestamp rendering shared by every context
that puts a time into a JSON field or a store record."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour minute second)))

(defun octal-mode (mode)
  "MODE's permission bits as a four-digit octal string, `0644`."
  (format nil "~4,'0O" (logand mode #o7777)))
