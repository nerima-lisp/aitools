;;;;
;;;; A delimiter scanner for Lisp-family source, shared by `check --format
;;;; lisp` (balance with strings, comments, and character literals
;;;; taken into account) and by `--symbol`'s :SEXP extent estimate. It is a
;;;; lexer for delimiters only; it does not read forms.
;;;;
;;;; Dialect differences it knows about:
;;;;   common-lisp  `#\x` characters, `|...|` symbols, `#|...|#` nested
;;;;                comments; only ( ) delimit.
;;;;   scheme       as common-lisp, plus [ ] and { }.
;;;;   clojure      `\x` characters; ( ) [ ] { }.
;;;;   emacs-lisp   `?x` and `?\x` characters; ( ) [ ].
(in-package #:aitools.inspect.domain)

(defun lisp-dialect-for-language (name)
  "The scanner dialect keyword for a text-context language NAME, or NIL
when NAME is not a Lisp family language."
  (cdr (assoc name '(("common-lisp" . :common-lisp) ("emacs-lisp" . :emacs-lisp)
                     ("scheme" . :scheme) ("clojure" . :clojure))
              :test #'string=)))

(defun %dialect-delimiters (dialect)
  "(VALUES openers closers) as strings, paired by position."
  (ecase dialect
    (:common-lisp (values "(" ")"))
    ((:scheme :clojure) (values "([{" ")]}"))
    (:emacs-lisp (values "([" ")]"))))

(defun %token-start-p (line col)
  (or (zerop col)
      (let ((previous (char line (1- col))))
        (or (member previous '(#\Space #\Tab #\( #\[ #\{ #\' #\` #\,))
            (char= previous #\Return)))))

(defstruct (%lisp-scan-state (:constructor %make-lisp-scan-state ()) (:copier nil) (:predicate nil))
  (mode :code)
  (line 0)
  (col 0)
  (comment-depth 0))

(defun %begin-lisp-scan (scanner mode line col)
  (setf (%lisp-scan-state-mode scanner) mode
        (%lisp-scan-state-line scanner) line
        (%lisp-scan-state-col scanner) col))

(defun %scan-code-character
    (scanner char line line-index col dialect openers closers emit
             block-comments bar-symbols)
  (let ((length (length line)))
    (cond
      ((char= char #\;) length)
      ((char= char #\")
       (%begin-lisp-scan scanner :string line-index col)
       (1+ col))
      ((and bar-symbols (char= char #\|))
       (%begin-lisp-scan scanner :bar line-index col)
       (1+ col))
      ((and block-comments (char= char #\#) (< (1+ col) length)
            (char= (char line (1+ col)) #\|))
       (%begin-lisp-scan scanner :block line-index col)
       (setf (%lisp-scan-state-comment-depth scanner) 1)
       (+ col 2))
      ((and (member dialect '(:common-lisp :scheme))
            (char= char #\#) (< (1+ col) length)
            (char= (char line (1+ col)) #\\))
       (+ col 3))
      ((and (eq dialect :clojure) (char= char #\\))
       (+ col 2))
      ((and (eq dialect :emacs-lisp) (char= char #\?)
            (%token-start-p line col))
       (+ col
          (if (and (< (1+ col) length)
                   (char= (char line (1+ col)) #\\))
              3
              2)))
      ((find char openers)
       (if (eq (funcall emit :open char line-index col) :stop)
           :stop
           (1+ col)))
      ((find char closers)
       (if (eq (funcall emit :close char line-index col) :stop)
           :stop
           (1+ col)))
      (t (1+ col)))))

(defun %scan-quoted-character (scanner char col closing)
  (cond
    ((char= char #\\) (+ col 2))
    ((char= char closing)
     (setf (%lisp-scan-state-mode scanner) :code)
     (1+ col))
    (t (1+ col))))

(defun %scan-block-comment-character (scanner char line col)
  (let ((length (length line)))
    (cond
      ((and (char= char #\|) (< (1+ col) length)
            (char= (char line (1+ col)) #\#))
       (decf (%lisp-scan-state-comment-depth scanner))
       (when (zerop (%lisp-scan-state-comment-depth scanner))
         (setf (%lisp-scan-state-mode scanner) :code))
       (+ col 2))
      ((and (char= char #\#) (< (1+ col) length)
            (char= (char line (1+ col)) #\|))
       (incf (%lisp-scan-state-comment-depth scanner))
       (+ col 2))
      (t (1+ col)))))

(defun %scan-lisp-state-character
     (scanner char line line-index col dialect openers closers emit
      block-comments bar-symbols)
  (ecase (%lisp-scan-state-mode scanner)
    (:code
     (%scan-code-character scanner char line line-index col dialect
                           openers closers emit block-comments bar-symbols))
    (:string (%scan-quoted-character scanner char col #\"))
    (:bar (%scan-quoted-character scanner char col #\|))
    (:block (%scan-block-comment-character scanner char line col))))

(defun scan-lisp-delimiters (lines dialect emit &key (start-line 0))
  "Walk LINES (a vector of strings) from index START-LINE, calling EMIT with
(:OPEN char line col) or (:CLOSE char line col) for each delimiter outside
strings, comments, and character literals; LINE is 0-based, COL 0-based.
EMIT returning :STOP ends the walk and returns :STOPPED. Otherwise returns
NIL, or (:STRING line col) / (:BLOCK-COMMENT line col) / (:ESCAPED-SYMBOL
line col) naming where an unterminated construct began."
  (declare (type function emit))
  (multiple-value-bind (openers closers) (%dialect-delimiters dialect)
    (let ((scanner (%make-lisp-scan-state))
          (block-comments (member dialect '(:common-lisp :scheme)))
          (bar-symbols (member dialect '(:common-lisp :scheme))))
      (loop for line-index from start-line below (length lines)
            for line = (svref lines line-index)
            do (loop with col = 0
                     while (< col (length line))
                     do (let ((next-col
                                (%scan-lisp-state-character
                                 scanner (char line col) line line-index col
                                 dialect openers closers emit
                                 block-comments bar-symbols)))
                          (when (eq next-col :stop)
                            (return-from scan-lisp-delimiters :stopped))
                          (setf col next-col))))
      (ecase (%lisp-scan-state-mode scanner)
        (:code nil)
        (:string
         (list :string (%lisp-scan-state-line scanner)
               (%lisp-scan-state-col scanner)))
        (:bar
         (list :escaped-symbol (%lisp-scan-state-line scanner)
               (%lisp-scan-state-col scanner)))
        (:block
         (list :block-comment (%lisp-scan-state-line scanner)
               (%lisp-scan-state-col scanner)))))))

(defun lisp-balance-diagnostics (lines dialect &key (limit 20))
  "Delimiter problems in LINES as a list of (line col message), 1-based
positions, in source order, at most LIMIT of them."
  (multiple-value-bind (openers closers) (%dialect-delimiters dialect)
    (let ((stack '()) (problems '()))
      (flet ((problem (line col message)
               (push (list (1+ line) (1+ col) message) problems)))
        (let ((unterminated
                (scan-lisp-delimiters
                 lines dialect
                 (lambda (kind char line col)
                   (ecase kind
                     (:open (push (list char line col) stack))
                     (:close
                      (let ((expected (and stack (char closers (position (first (first stack)) openers)))))
                        (cond ((null stack)
                               (problem line col (format nil "unexpected '~C' with no open delimiter" char)))
                              ((char= char expected) (pop stack))
                              (t
                               (destructuring-bind (open open-line open-col) (pop stack)
                                 (problem line col
                                          (format nil "'~C' closes '~C' opened at line ~D, column ~D (expected '~C')"
                                                  char open (1+ open-line) (1+ open-col) expected))))))))
                   nil))))
          (when unterminated
            (destructuring-bind (kind line col) unterminated
              (problem line col (ecase kind
                                  (:string "unterminated string")
                                  (:escaped-symbol "unterminated |...| symbol")
                                  (:block-comment "unterminated #| comment")))))
          (dolist (open (reverse stack))
            (destructuring-bind (char line col) open
              (problem line col (format nil "'~C' is never closed" char))))))
      (let ((sorted (stable-sort (nreverse problems)
                                 (lambda (a b) (or (< (first a) (first b))
                                                   (and (= (first a) (first b)) (< (second a) (second b))))))))
        (subseq sorted 0 (min limit (length sorted)))))))

(defun sexp-end-line (lines start-line dialect)
  "The 0-based index of the line where the first form opening on or after
START-LINE closes, or the last line when it never closes."
  (let ((depth 0) (end nil))
    (scan-lisp-delimiters lines dialect
                          (lambda (kind char line col)
                            (declare (ignore char col))
                            (if (eq kind :open)
                                (progn (incf depth) nil)
                                (when (and (plusp depth) (zerop (decf depth)))
                                  (setf end line)
                                  :stop)))
                          :start-line start-line)
    (or end (1- (length lines)))))
