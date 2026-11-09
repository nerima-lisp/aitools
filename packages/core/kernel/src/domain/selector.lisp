;;;;
;;;; The five selectors (docs/src/reference/commands/index.md, Conventions shared
;;;; by many commands), as one value type plus the data
;;;; table every command's presentation layer consults to enforce "which
;;;; selectors does this command accept" and "is a multiple match an error or
;;;; the whole match set". Resolving a selector against real file content
;;;; (finding the matching line range) is command-specific and lives in each
;;;; feature's domain layer; this file only models the selector itself.
(in-package #:aitools.kernel.domain)

(defstruct (selector
            (:constructor %make-selector
                (kind &key range-start range-end symbol-name symbol-kind
                   between-start between-end exclusive match-pattern invert))
            (:copier nil))
  "KIND is one of :OLD :RANGE :SYMBOL :BETWEEN :MATCH. Only the slots that
KIND uses are meaningful; the rest stay NIL."
  (kind nil :type (member :old :range :symbol :between :match) :read-only t)
  (range-start nil :type (or null (integer 1)) :read-only t)
  (range-end nil :type (or null (integer 1)) :read-only t)
  (symbol-name nil :type (or null string) :read-only t)
  (symbol-kind nil :type (or null string) :read-only t)
  (between-start nil :type (or null string) :read-only t)
  (between-end nil :type (or null string) :read-only t)
  (exclusive nil :type boolean :read-only t)
  (match-pattern nil :type (or null string) :read-only t)
  (invert nil :type boolean :read-only t))

(defun %ascii-digit-char-p (char)
  "True for `0`-`9` only. DIGIT-CHAR-P also accepts other scripts' decimal
digits (`٣`), which no command-line number allows."
  (char<= #\0 char #\9))

(defun parse-range-spec (spec)
  "Parse a `--range` argument: `S:E`, `S:` (S to end of file), or `N` (N:N).
Returns (VALUES START END) with END NIL meaning \"to the end of the file\".
Both are 1-based, inclusive, and spelled in ASCII digits only (no sign, no
whitespace). Signals a SIMPLE-ERROR on a malformed spec; the presentation
layer that owns ARGUMENT.INVALID formatting catches it."
  (let ((colon (position #\: spec)))
    (flet ((parse-line-number (text)
             (if (and (plusp (length text)) (every #'%ascii-digit-char-p text))
                 (parse-integer text)
                 (error "not a line number: ~S" text))))
      (cond
        ((null colon)
         (let ((n (parse-line-number spec)))
           (when (< n 1) (error "line numbers start at 1: ~S" spec))
           (values n n)))
        (t
         (let* ((start-text (subseq spec 0 colon))
                (end-text (subseq spec (1+ colon))))
           (when (zerop (length start-text))
             (error "a --range spec must give a start line: ~S" spec))
           (let ((start (parse-line-number start-text)))
             (when (< start 1) (error "line numbers start at 1: ~S" spec))
             (if (zerop (length end-text))
                 (values start nil)
                 (let ((end (parse-line-number end-text)))
                   (when (< end start)
                     (error "range end ~A is before start ~A" end start))
                   (values start end))))))))))

(defmacro define-selector-constructor (name lambda-list kind &body arguments)
  `(defun ,name ,lambda-list
     (%make-selector ,kind ,@arguments)))

(defun make-range-selector (spec)
  (multiple-value-bind (start end) (parse-range-spec spec)
    (%make-selector :range :range-start start :range-end end)))

(define-selector-constructor make-symbol-selector (name &key kind) :symbol
  :symbol-name name
  :symbol-kind kind)

(define-selector-constructor make-between-selector
    (start-pattern end-pattern &key exclusive) :between
  :between-start start-pattern
  :between-end end-pattern
  :exclusive (and exclusive t))

(define-selector-constructor make-match-selector (pattern &key invert) :match
  :match-pattern pattern
  :invert (and invert t))

(defstruct (selector-catalog-entry (:copier nil) (:predicate nil))
  (kind nil :read-only t)
  (basis nil :type (member :content :position) :read-only t)
  (uniqueness nil :type (member :ambiguous :select-all :fixed) :read-only t))

(defparameter *selector-catalog*
  (loop for (kind basis uniqueness) in *kernel-selector-catalog-data*
        collect (make-selector-catalog-entry :kind kind
                                             :basis basis
                                             :uniqueness uniqueness))
  "One entry per selector, in the order --old, --range, --symbol, --between,
--match.")

(defun %selector-catalog-entry (kind)
  (or (find kind *selector-catalog* :key #'selector-catalog-entry-kind)
      (error "unknown selector kind ~S" kind)))

(defun selector-basis (selector-or-kind)
  (selector-catalog-entry-basis
   (%selector-catalog-entry
    (if (selector-p selector-or-kind) (selector-kind selector-or-kind) selector-or-kind))))

(defun selector-uniqueness (selector-or-kind)
  (selector-catalog-entry-uniqueness
   (%selector-catalog-entry
    (if (selector-p selector-or-kind) (selector-kind selector-or-kind) selector-or-kind))))

(defparameter *selector-command-acceptance* *kernel-selector-command-acceptance-data*)

(defun selector-accepts-command-p (command kind)
  "True when COMMAND (a keyword such as :EDIT) accepts a selector of KIND."
  (member kind (cdr (assoc command *selector-command-acceptance*)) :test #'eq))
