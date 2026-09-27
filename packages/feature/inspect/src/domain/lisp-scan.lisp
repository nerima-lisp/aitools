;;;; packages/feature/inspect/src/domain/lisp-scan.lisp
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

(defun scan-lisp-delimiters (lines dialect emit &key (start-line 0))
  "Walk LINES (a vector of strings) from index START-LINE, calling EMIT with
(:OPEN char line col) or (:CLOSE char line col) for each delimiter outside
strings, comments, and character literals; LINE is 0-based, COL 0-based.
EMIT returning :STOP ends the walk and returns :STOPPED. Otherwise returns
NIL, or (:STRING line col) / (:BLOCK-COMMENT line col) / (:ESCAPED-SYMBOL
line col) naming where an unterminated construct began."
  (declare (type function emit))
  (multiple-value-bind (openers closers) (%dialect-delimiters dialect)
    (let ((state :code) (state-line 0) (state-col 0) (comment-depth 0)
          (block-comments (member dialect '(:common-lisp :scheme)))
          (bar-symbols (member dialect '(:common-lisp :scheme))))
      (loop for line-index from start-line below (length lines)
            for line = (svref lines line-index)
            for length = (length line)
            do (let ((col 0))
                 (flet ((begin (new-state)
                          (setf state new-state state-line line-index state-col col)))
                   (loop while (< col length)
                         do (let ((char (char line col)))
                              (ecase state
                                (:code
                                 (cond
                                   ((char= char #\;) (setf col length))
                                   ((char= char #\") (begin :string) (incf col))
                                   ((and bar-symbols (char= char #\|)) (begin :bar) (incf col))
                                   ((and block-comments (char= char #\#) (< (1+ col) length)
                                         (char= (char line (1+ col)) #\|))
                                    (begin :block) (setf comment-depth 1) (incf col 2))
                                   ((and (member dialect '(:common-lisp :scheme)) (char= char #\#)
                                         (< (1+ col) length) (char= (char line (1+ col)) #\\))
                                    (incf col 3))
                                   ((and (eq dialect :clojure) (char= char #\\)) (incf col 2))
                                   ((and (eq dialect :emacs-lisp) (char= char #\?) (%token-start-p line col))
                                    (incf col (if (and (< (1+ col) length) (char= (char line (1+ col)) #\\)) 3 2)))
                                   ((find char openers)
                                    (when (eq (funcall emit :open char line-index col) :stop)
                                      (return-from scan-lisp-delimiters :stopped))
                                    (incf col))
                                   ((find char closers)
                                    (when (eq (funcall emit :close char line-index col) :stop)
                                      (return-from scan-lisp-delimiters :stopped))
                                    (incf col))
                                   (t (incf col))))
                                (:string
                                 (cond ((char= char #\\) (incf col 2))
                                       ((char= char #\") (setf state :code) (incf col))
                                       (t (incf col))))
                                (:bar
                                 (cond ((char= char #\\) (incf col 2))
                                       ((char= char #\|) (setf state :code) (incf col))
                                       (t (incf col))))
                                (:block
                                 (cond ((and (char= char #\|) (< (1+ col) length) (char= (char line (1+ col)) #\#))
                                        (decf comment-depth) (incf col 2)
                                        (when (zerop comment-depth) (setf state :code)))
                                       ((and (char= char #\#) (< (1+ col) length) (char= (char line (1+ col)) #\|))
                                        (incf comment-depth) (incf col 2))
                                       (t (incf col))))))))))
      (ecase state
        (:code nil)
        (:string (list :string state-line state-col))
        (:bar (list :escaped-symbol state-line state-col))
        (:block (list :block-comment state-line state-col))))))

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
