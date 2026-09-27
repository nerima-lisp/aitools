;;;; packages/feature/inspect/src/application/selector.lisp
;;;;
;;;; The public selector boundary (selector rules: docs/src/reference/commands.md,
;;;; "Conventions shared by many commands"): turning
;;;; parsed options into one kernel SELECTOR under the "one selector per
;;;; call" rule, and resolving it against a file's lines. `read` and
;;;; `archive read` use it here; the edit and vcs contexts reach it through
;;;; this package, the only part of inspect they may reference.
(in-package #:aitools.inspect.application)

(defun %option-conflict-repair (command)
  (repair "use-one-selector" "Choose exactly one selector for this call."
          (format nil "aitools schema ~A" (string-downcase (substitute #\Space #\- (symbol-name command) :count 1)))))

(defun parse-selector-options/k (command &key range symbol kind between exclusive match invert extra-exclusive
                                              on-selector on-none on-invalid)
  "Build the one selector of COMMAND's options and call
ON-SELECTOR (selector), ON-NONE (), or ON-INVALID (message repairs) when two
selectors are given, a modifier lacks its selector, a kind is not accepted
by COMMAND, or a value is malformed. EXTRA-EXCLUSIVE is an alist of
(flag-text . value) for further flags that count toward the one-per-call
rule; a NIL value means the flag is absent."
  (declare (type function on-selector on-none on-invalid))
  (let* ((given (remove nil (list (and range (cons "--range" :range))
                                  (and symbol (cons "--symbol" :symbol))
                                  (and between (cons "--between" :between))
                                  (and match (cons "--match" :match)))))
         (extras (loop for (flag . value) in extra-exclusive when value collect flag))
         (named (append (mapcar #'car given) extras))
         (repairs (list (%option-conflict-repair command))))
    (flet ((invalid (format-control &rest arguments)
             (return-from parse-selector-options/k
               (funcall on-invalid (apply #'format nil format-control arguments) repairs))))
      (when (> (length named) 1)
        (invalid "~{~A~^, ~} cannot be combined: one selector per call" named))
      (when (and exclusive (not between)) (invalid "--exclusive needs --between"))
      (when (and invert (not match)) (invalid "--invert needs --match"))
      (when (and kind (not symbol)) (invalid "--kind needs --symbol"))
      (when (and given (not (selector-accepts-command-p command (cdr (first given)))))
        (invalid "~A does not accept ~A" (string-downcase (symbol-name command)) (car (first given))))
      (cond
        ((null given) (funcall on-none))
        (range
         (let ((selector (handler-case (make-range-selector range)
                           (error () (invalid "--range ~S is not S:E, S:, or N (1-based lines)" range)))))
           (funcall on-selector selector)))
        (symbol
         (when (zerop (length symbol)) (invalid "--symbol needs a name"))
         (funcall on-selector (make-symbol-selector symbol :kind kind)))
        (between
         (unless (and (listp between) (= (length between) 2) (every #'stringp between)
                      (every #'plusp (mapcar #'length between)))
           (invalid "--between needs a start and an end regular expression"))
         (funcall on-selector (make-between-selector (first between) (second between) :exclusive exclusive)))
        (t
         (when (zerop (length match)) (invalid "--match needs a regular expression"))
         (funcall on-selector (make-match-selector match :invert invert)))))))

(defun resolve-selector/k (lines selector &key path on-selected on-no-match on-ambiguous on-invalid)
  "Resolve SELECTOR (any kernel SELECTOR but :OLD) against LINES, a
simple-vector of line strings. PATH chooses the language for :SYMBOL.
Calls exactly one of ON-SELECTED (ranges), ON-NO-MATCH (candidates),
ON-AMBIGUOUS (candidates), ON-INVALID (code message)."
  (resolve-line-selector/k lines selector
                           :language (and path (language-for-path path))
                           :on-selected on-selected :on-no-match on-no-match
                           :on-ambiguous on-ambiguous :on-invalid on-invalid))

(defun selection-error (on-error context selection-kind candidates &key path code message)
  "Report a failed selection: SELECTION-KIND :NO-MATCH or :AMBIGUOUS, or
:INVALID with CODE and MESSAGE from RESOLVE-SELECTOR/K."
  (ecase selection-kind
    (:no-match
     (fail on-error "selection.no-match" (format nil "the selector matched nothing in ~A" path)
           :candidates candidates
           :repairs (list (repair "read-file" "Read the file to choose a selector."
                                  (command-line context (list "read" path))))))
    (:ambiguous
     (fail on-error "selection.ambiguous"
           (format nil "the selector matched ~D places in ~A; it must match one" (length candidates) path)
           :candidates candidates
           :repairs (list (repair "use-range" "Select one match by its line range."
                                  (command-line context (list "read" path "--range"
                                                              (format nil "~D:~D"
                                                                      (json-object-get (first candidates) "line")
                                                                      (json-object-get (first candidates) "end_line"))))))))
    (:invalid
     (fail on-error code message
           :repairs (list (if (string= code "input.unsupported-language")
                              (repair "use-range" "Select lines by number instead."
                                      (command-line context (list "read" path "--range" "1:80")))
                              (repair "fix-pattern" "Correct the regular expression; see the syntax rules."
                                      "aitools schema read")))))))
