;;;; packages/feature/inspect/src/domain/selection.lisp
;;;;
;;;; Resolving the line selectors (`--range`, `--symbol`, `--between`,
;;;; `--match`) against a file's lines. The outcome is a three-way branch
;;;; (selected, several, none), so this
;;;; is a /K function; the application layer publishes it to the edit and
;;;; vcs contexts as RESOLVE-SELECTOR/K.
(in-package #:aitools.inspect.domain)

(defconstant +selection-candidate-count+ 3)

(defun %line-candidate (lines number)
  (json-object "line" number "text" (svref lines (1- number))))

(defun %span-candidate (lines start end)
  (json-object "line" start "end_line" end "text" (svref lines (1- start))))

(defun %similar-line-candidates (lines text)
  "Up to three lines nearest to TEXT, as {line,text} objects."
  (let ((numbered (loop for index from 0 below (length lines)
                        unless (%blank-line-p (svref lines index))
                          collect (1+ index))))
    (mapcar (lambda (number) (%line-candidate lines number))
            (rank-similar (string-trim '(#\Space #\Tab) text) numbered
                          :key (lambda (number) (string-trim '(#\Space #\Tab) (svref lines (1- number))))
                          :count +selection-candidate-count+))))

(defun %pattern-literal (pattern)
  "PATTERN with regex syntax characters removed, to rank similar lines."
  (remove-if (lambda (char) (find char "\\^$.|?*+()[]{}")) pattern))

(defun %resolve-range (lines selector on-selected on-no-match)
  (let ((total (length lines))
        (start (selector-range-start selector))
        (end (selector-range-end selector)))
    (if (> start total)
        (funcall on-no-match (if (plusp total) (list (%line-candidate lines total)) '()))
        (funcall on-selected (list (cons start (min total (or end total))))))))

(defun %resolve-match (lines selector on-selected on-no-match on-invalid)
  (compile-pattern/k
   (selector-match-pattern selector)
   :on-error (lambda (message) (funcall on-invalid "input.syntax-error" message))
   :on-compiled
   (lambda (regex)
     (let ((invert (selector-invert selector)) (selected nil) (limit-message nil))
       (call-with-regex-limit/k
        (lambda ()
          (setf selected (loop for index from 0 below (length lines)
                               when (if (pattern-matches-p regex (svref lines index)) (not invert) invert)
                                 collect (cons (1+ index) (1+ index)))))
        (lambda (message) (setf limit-message message)))
       (cond (limit-message
              (funcall on-invalid "input.syntax-error"
                       (format nil "the --match pattern is too complex to evaluate: ~A" limit-message)))
             (selected (funcall on-selected selected))
             (t (funcall on-no-match (%similar-line-candidates lines (%pattern-literal (selector-match-pattern selector))))))))))

(defun %resolve-between (lines selector on-selected on-no-match on-ambiguous on-invalid)
  (flet ((compile-both (continuation)
           (compile-pattern/k
            (selector-between-start selector)
            :on-error (lambda (message) (funcall on-invalid "input.syntax-error" message))
            :on-compiled
            (lambda (start-regex)
              (compile-pattern/k
               (selector-between-end selector)
               :on-error (lambda (message) (funcall on-invalid "input.syntax-error" message))
               :on-compiled (lambda (end-regex) (funcall continuation start-regex end-regex)))))))
    (compile-both
     (lambda (start-regex end-regex)
       (let ((blocks nil) (limit-message nil))
         (call-with-regex-limit/k
          (lambda ()
            (setf blocks
                  (loop for index from 0 below (length lines)
                        when (pattern-matches-p start-regex (svref lines index))
                          append (let ((end (loop for later from (1+ index) below (length lines)
                                                  when (pattern-matches-p end-regex (svref lines later))
                                                    return later)))
                                   (and end (list (cons (1+ index) (1+ end))))))))
          (lambda (message) (setf limit-message message)))
         (cond
           (limit-message
            (funcall on-invalid "input.syntax-error"
                     (format nil "the --between pattern is too complex to evaluate: ~A" limit-message)))
           ((null blocks)
            (funcall on-no-match
                     (%similar-line-candidates lines (%pattern-literal (selector-between-start selector)))))
           ((rest blocks)
            (funcall on-ambiguous (loop for (start . end) in blocks collect (%span-candidate lines start end))))
           (t
            (destructuring-bind ((start . end)) blocks
              (funcall on-selected
                       (list (if (selector-exclusive selector)
                                 (cons (1+ start) (1- end))
                                 (cons start end))))))))))))

(defun %resolve-symbol (lines selector language on-selected on-no-match on-ambiguous on-invalid)
  (if (or (null language) (null (language-definitions language)))
      (funcall on-invalid "input.unsupported-language"
               "--symbol needs a file in a language of the definition table")
      (let* ((definitions (find-definitions lines language))
             (name (selector-symbol-name selector))
             (kind (selector-symbol-kind selector))
             (hits (remove-if-not (lambda (definition)
                                    (and (string= (definition-name definition) name)
                                         (or (null kind) (string= (definition-kind definition) kind))))
                                  definitions)))
        (cond
          ((null hits)
           (funcall on-no-match
                    (mapcar (lambda (definition)
                              (json-object "line" (definition-line definition)
                                           "end_line" (definition-end-line definition)
                                           "kind" (definition-kind definition)
                                           "name" (definition-name definition)))
                            (rank-similar name definitions :key #'definition-name
                                                           :count +selection-candidate-count+))))
          ((rest hits)
           (funcall on-ambiguous
                    (mapcar (lambda (definition)
                              (json-object "line" (definition-line definition)
                                           "end_line" (definition-end-line definition)
                                           "kind" (definition-kind definition)
                                           "name" (definition-name definition)))
                            hits)))
          (t
           (funcall on-selected
                    (list (cons (definition-line (first hits)) (definition-end-line (first hits))))))))))

(defun resolve-line-selector/k (lines selector &key language on-selected on-no-match on-ambiguous on-invalid)
  "Resolve SELECTOR (a kernel SELECTOR other than :OLD) against LINES and
call exactly one continuation: ON-SELECTED (ranges), ON-NO-MATCH
(candidates), ON-AMBIGUOUS (candidates), or ON-INVALID (code message).
RANGES are (START . END) conses, 1-based and inclusive, in file order;
LANGUAGE (a text-context LANGUAGE or NIL) serves :SYMBOL."
  (declare (type function on-selected on-no-match on-ambiguous on-invalid))
  (ecase (selector-kind selector)
    (:range (%resolve-range lines selector on-selected on-no-match))
    (:match (%resolve-match lines selector on-selected on-no-match on-invalid))
    (:between (%resolve-between lines selector on-selected on-no-match on-ambiguous on-invalid))
    (:symbol (%resolve-symbol lines selector language on-selected on-no-match on-ambiguous on-invalid))))
