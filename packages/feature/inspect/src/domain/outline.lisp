;;;; packages/feature/inspect/src/domain/outline.lisp
;;;;
;;;; Definitions for `--symbol`: the start line comes from
;;;; the text context's language table (the same data `code outline` uses),
;;;; the end line from the language's :EXTENT rule. Feature domains cannot
;;;; share code, so the search context's outline and this one read the same
;;;; table independently.
(in-package #:aitools.inspect.domain)

(defstruct (definition (:constructor make-definition (line end-line kind name)) (:copier nil))
  "LINE and END-LINE are 1-based and inclusive."
  (line 0 :type (integer 1) :read-only t)
  (end-line 0 :type (integer 1) :read-only t)
  (kind "" :type string :read-only t)
  (name "" :type string :read-only t))

(defvar *compiled-definitions* (make-hash-table :test 'equal)
  "Language name -> ((kind . regex) ...), compiled on first use so that
startup does no regex work.")

(defun %language-patterns (language)
  (let ((name (language-name language)))
    (or (gethash name *compiled-definitions*)
        (setf (gethash name *compiled-definitions*)
              (loop for (kind pattern) in (language-definitions language)
                    collect (cons kind (cl-regex-kit:compile-regex pattern)))))))

(defun %leading-width (line)
  "Indentation width of LINE, a tab counting to the next multiple of 8."
  (let ((width 0))
    (loop for char across line
          do (case char
               (#\Space (incf width))
               (#\Tab (setf width (* 8 (1+ (floor width 8)))))
               (t (return))))
    width))

(defun %blank-line-p (line)
  (every (lambda (char) (member char '(#\Space #\Tab #\Return))) line))

(defun %indent-end (lines start)
  "Last line of the block opened at START: up to the line before the next
non-blank line indented no deeper than START, trailing blank lines left out."
  (let ((indent (%leading-width (svref lines start)))
        (last start))
    (loop for index from (1+ start) below (length lines)
          for line = (svref lines index)
          do (unless (%blank-line-p line)
               (when (<= (%leading-width line) indent) (return))
               (setf last index)))
    last))

(defun %heading-level (line)
  (let ((hashes (or (position #\# line :test-not #'char=) (length line))))
    (and (<= 1 hashes 6)
         (or (= hashes (length line)) (member (char line hashes) '(#\Space #\Tab)))
         hashes)))

(defun %fence-line-p (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (or (eql 0 (search "```" trimmed)) (eql 0 (search "~~~" trimmed)))))

(defun %heading-end (lines start)
  "Line before the next heading of the same or a higher level outside code
fences, or the last line."
  (let ((level (or (%heading-level (svref lines start)) 1))
        (in-fence nil))
    (loop for index from (1+ start) below (length lines)
          for line = (svref lines index)
          do (cond ((%fence-line-p line) (setf in-fence (not in-fence)))
                   ((and (not in-fence) (%heading-level line) (<= (%heading-level line) level))
                    (return-from %heading-end (1- index)))))
    (1- (length lines))))

(defconstant +brace-search-lines+ 50
  "How far past a brace-language definition line to look for its `{`
before treating the definition as a one-liner.")

(defun %brace-end (lines start language)
  "Close of the first `{` block at or after START, skipping strings and
comments; a `;` at bracket depth 0 before any `{` ends a bodiless
definition there."
  (let ((line-comment (language-line-comment language))
        (block-open (first (language-block-comment language)))
        (block-close (second (language-block-comment language)))
        (quotes (if (string= (language-name language) "rust") "\"" "\"'`"))
        (depth 0) (parens 0) (opened nil) (quote nil) (in-block nil))
    (flet ((at (line col text) (and text (string= text line :start2 col :end2 (min (length line) (+ col (length text)))))))
      (loop for index from start below (length lines)
            for line = (svref lines index)
            do (when (and (not opened) (> index (+ start +brace-search-lines+)))
                 (return-from %brace-end start))
               (let ((col 0) (length (length line)))
                 (loop while (< col length)
                       do (let ((char (char line col)))
                            (cond
                              (in-block
                               (if (at line col block-close)
                                   (progn (setf in-block nil) (incf col (length block-close)))
                                   (incf col)))
                              (quote
                               (cond ((char= char #\\) (incf col 2))
                                     ((char= char quote) (setf quote nil) (incf col))
                                     (t (incf col))))
                              ((and line-comment (at line col line-comment)) (setf col length))
                              ((and block-open (at line col block-open))
                               (setf in-block t) (incf col (length block-open)))
                              ((find char quotes) (setf quote char) (incf col))
                              ((char= char #\{) (incf depth) (setf opened t) (incf col))
                              ((char= char #\})
                               (decf depth)
                               (when (and opened (<= depth 0)) (return-from %brace-end index))
                               (incf col))
                              ((char= char #\() (incf parens) (incf col))
                              ((char= char #\)) (decf parens) (incf col))
                              ((and (char= char #\;) (not opened) (<= depth 0) (<= parens 0))
                               (return-from %brace-end index))
                              (t (incf col)))))
                 ;; A line comment or unterminated quote does not carry
                 ;; over; only block comments span lines.
                 (setf quote nil)))
      (if opened (1- (length lines)) start))))

(defun definition-end-index (lines start language)
  "The 0-based last line of the definition starting at 0-based START."
  (ecase (language-extent language)
    (:sexp (sexp-end-line lines start (or (lisp-dialect-for-language (language-name language)) :common-lisp)))
    (:brace (%brace-end lines start language))
    (:indent (%indent-end lines start))
    (:heading (%heading-end lines start))))

(defun find-definitions (lines language)
  "Every definition in LINES recognised by LANGUAGE's patterns, in line
order. The first pattern (in table order) matching a line names it. Markdown
headings inside code fences are not definitions."
  (let ((patterns (%language-patterns language))
        (fenced (eq (language-extent language) :heading))
        (in-fence nil))
    (loop for index from 0 below (length lines)
          for line = (svref lines index)
          for fence = (and fenced (%fence-line-p line))
          for inside = (if fence (setf in-fence (not in-fence)) in-fence)
          for hit = (and (not fence) (not inside)
                         (loop for (kind . regex) in patterns
                          for name = (pattern-group-string regex line "name")
                          when name return (cons kind name)))
          when hit
            collect (make-definition (1+ index) (1+ (definition-end-index lines index language))
                                     (car hit) (cdr hit)))))
