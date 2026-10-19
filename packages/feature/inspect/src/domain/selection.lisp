;;;; packages/feature/inspect/src/domain/selection.lisp
;;;;
;;;; Resolves line selectors (`--range`, `--symbol`, `--between`, `--match`)
;;;; against file lines. Results are selected ranges, no-match candidates,
;;;; ambiguous candidates, or an invalid selector code and message. The
;;;; application layer exposes this as RESOLVE-SELECTOR/K to edit and VCS.
(in-package #:aitools.inspect.domain)

(defconstant +selection-candidate-count+
  3)

(defun %line-candidate (lines number)
  (json-object "line" number "text" (svref lines (1- number))))

(defun %span-candidate (lines start end)
  (json-object "line" start "end_line" end "text" (svref lines (1- start))))

(defun %similar-line-candidates (lines text)
  "Up to three lines nearest to TEXT, as {line,text} objects."
  (let ((numbered
         (loop for index from 0 below (length lines)
               unless (%blank-line-p (svref lines index))
                 collect (1+ index))))
    (mapcar
     (lambda (number)
       (%line-candidate lines number))
     (rank-similar (string-trim '(#\Space #\Tab) text)
                   numbered
                   :key
                   (lambda (number)
                     (string-trim '(#\Space #\Tab) (svref lines (1- number))))
                   :count
                   +selection-candidate-count+))))

(defun %pattern-literal (pattern)
  "PATTERN with regex syntax characters removed, to rank similar lines."
  (remove-if
   (lambda (char)
     (find char "\\^$.|?*+()[]{}"))
   pattern))

(aitools.kernel.domain:define-flow
 (%resolve-range lines selector &key on-selected on-no-match)
 (let ((total (length lines))
       (start (selector-range-start selector))
       (end (selector-range-end selector)))
   (if (> start total)
       (aitools.kernel.domain:flow-continue :on-no-match
                                            (if (plusp total)
                                                (list
                                                 (%line-candidate lines total))
                                                '()))
       (aitools.kernel.domain:flow-continue :on-selected
                                            (list
                                             (cons start
                                                   (min total (or end total))))))))

(aitools.kernel.domain:define-flow
 (%resolve-match lines selector &key on-selected on-no-match on-invalid)
 (aitools.kernel.domain:flow-call
  (compile-pattern/k (selector-match-pattern selector))
  (:on-error (message)
             (aitools.kernel.domain:flow-continue :on-invalid
                                                  "input.syntax-error"
                                                  message))
  (:on-compiled (regex)
                (let ((invert (selector-invert selector))
                      (selected nil)
                      (limit-message nil))
                  (aitools.kernel.domain:flow-call
                   (call-with-regex-limit/k
                    (lambda ()
                      (setf selected (loop for index from 0 below (length lines)
                                           when (if (pattern-matches-p regex
                                                                       (svref
                                                                        lines
                                                                        index))
                                                    (not invert)
                                                    invert)
                                             collect (cons (1+ index)
                                                           (1+ index))))))
                   (:on-limit (message)
                              (setf limit-message message)))
                  (cond
                    (limit-message
                     (aitools.kernel.domain:flow-continue :on-invalid
                                                          "input.syntax-error"
                                                          (format nil
                                                                  "the --match pattern is too complex to evaluate: ~A"
                                                                  limit-message)))
                    (selected
                     (aitools.kernel.domain:flow-continue :on-selected selected))
                    (t
                     (aitools.kernel.domain:flow-continue :on-no-match
                                                          (%similar-line-candidates
                                                           lines
                                                           (%pattern-literal
                                                            (selector-match-pattern
                                                             selector))))))))))

(aitools.kernel.domain:define-flow
 (%resolve-between lines
                   selector
                   &key
                   on-selected
                   on-no-match
                   on-ambiguous
                   on-invalid)
 (aitools.kernel.domain:flow-call
  (compile-pattern/k (selector-between-start selector))
  (:on-error (message)
             (aitools.kernel.domain:flow-continue :on-invalid
                                                  "input.syntax-error"
                                                  message))
  (:on-compiled (start-regex)
                (aitools.kernel.domain:flow-call
                 (compile-pattern/k (selector-between-end selector))
                 (:on-error (message)
                            (aitools.kernel.domain:flow-continue :on-invalid
                                                                 "input.syntax-error"
                                                                 message))
                 (:on-compiled (end-regex)
                               (let ((blocks nil)
                                     (limit-message nil))
                                 (aitools.kernel.domain:flow-call
                                  (call-with-regex-limit/k
                                   (lambda ()
                                     (setf blocks (loop for index from 0 below (length
                                                                                lines)
                                                        when (pattern-matches-p
                                                              start-regex
                                                              (svref lines
                                                                     index))
                                                          append (let ((end
                                                                        (loop for later from (1+
                                                                                              index) below (length
                                                                                                            lines)
                                                                              when (pattern-matches-p
                                                                                    end-regex
                                                                                    (svref
                                                                                     lines
                                                                                     later))
                                                                                return later)))
                                                                   (and end
                                                                        (list
                                                                         (cons
                                                                          (1+
                                                                           index)
                                                                          (1+
                                                                           end)))))))))
                                  (:on-limit (message)
                                             (setf limit-message message)))
                                 (cond
                                   (limit-message
                                    (aitools.kernel.domain:flow-continue
                                     :on-invalid
                                     "input.syntax-error"
                                     (format nil
                                             "the --between pattern is too complex to evaluate: ~A"
                                             limit-message)))
                                   ((null blocks)
                                    (aitools.kernel.domain:flow-continue
                                     :on-no-match
                                     (%similar-line-candidates lines
                                                               (%pattern-literal
                                                                (selector-between-start
                                                                 selector)))))
                                   ((rest blocks)
                                    (aitools.kernel.domain:flow-continue
                                     :on-ambiguous
                                     (loop for (start . end) in blocks
                                           collect (%span-candidate lines
                                                                    start
                                                                    end))))
                                   (t
                                    (destructuring-bind ((start . end)) blocks
                                      (aitools.kernel.domain:flow-continue
                                       :on-selected
                                       (list
                                        (if (selector-exclusive selector)
                                            (cons (1+ start) (1- end))
                                            (cons start end)))))))))))))
