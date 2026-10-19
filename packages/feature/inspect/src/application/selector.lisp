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
  (repair "use-one-selector"
          "Choose exactly one selector for this call."
          (format nil
                  "aitools schema ~A"
                  (string-downcase
                   (substitute #\Space #\- (symbol-name command) :count 1)))))

(defparameter *selector-option-conflict-form*
  '(lambda (count)
     (> count 1)))

(defparameter *selector-option-conflict-p*
  (compile nil *selector-option-conflict-form*))

(aitools.kernel.domain:define-flow
 (parse-selector-options/k command
                           &key
                           range
                           symbol
                           kind
                           between
                           exclusive
                           match
                           invert
                           extra-exclusive
                           on-selector
                           on-none
                           on-invalid)
 "Build the one selector for COMMAND. Call ON-SELECTOR with the selector,
ON-NONE when no selector is given, or ON-INVALID with a message and repairs
when options conflict, a modifier lacks its selector, a kind is not accepted,
or a value is malformed. EXTRA-EXCLUSIVE holds (flag-text . value) pairs;
NIL means the flag is absent."
 (declare (type function on-selector on-none on-invalid))
 (let* ((given
         (remove nil
                 (list (and range (cons "--range" :range))
                       (and symbol (cons "--symbol" :symbol))
                       (and between (cons "--between" :between))
                       (and match (cons "--match" :match)))))
        (extras
         (loop for (flag . value) in extra-exclusive
               when value
                 collect flag))
        (named (append (mapcar #'car given) extras))
        (repairs (list (%option-conflict-repair command))))
   (labels ((invalid (message)
              (aitools.kernel.domain:flow-continue :on-invalid message repairs)))
     (cond
       ((funcall *selector-option-conflict-p* (length named))
        (invalid
         (format nil
                 "~{~A~^, ~} cannot be combined: one selector per call"
                 named)))
       ((and exclusive (not between)) (invalid "--exclusive needs --between"))
       ((and invert (not match)) (invalid "--invert needs --match"))
       ((and kind (not symbol)) (invalid "--kind needs --symbol"))
       ((and given
             (not (selector-accepts-command-p command (cdr (first given)))))
        (invalid
         (format nil
                 "~A does not accept ~A"
                 (string-downcase (symbol-name command))
                 (car (first given)))))
       ((null given) (aitools.kernel.domain:flow-continue :on-none))
       (range
        (multiple-value-bind (selector valid)
            (handler-case (values (make-range-selector range) t)
              (error ()
                (values nil nil)))
          (if valid
              (aitools.kernel.domain:flow-continue :on-selector selector)
              (invalid
               (format nil
                       "--range ~S is not S:E, S:, or N (1-based lines)"
                       range)))))
       (symbol
        (if (zerop (length symbol))
            (invalid "--symbol needs a name")
            (aitools.kernel.domain:flow-continue :on-selector
                                                 (make-symbol-selector symbol
                                                                       :kind
                                                                       kind))))
       (between
        (if (and (listp between)
                 (= (length between) 2)
                 (every #'stringp between)
                 (every #'plusp (mapcar #'length between)))
            (aitools.kernel.domain:flow-continue :on-selector
                                                 (make-between-selector
                                                  (first between)
                                                  (second between)
                                                  :exclusive
                                                  exclusive))
            (invalid "--between needs a start and an end regular expression")))
       (t
        (if (zerop (length match))
            (invalid "--match needs a regular expression")
            (aitools.kernel.domain:flow-continue :on-selector
                                                 (make-match-selector match
                                                                      :invert
                                                                      invert))))))))

(aitools.kernel.domain:define-flow
 (resolve-selector/k lines
                     selector
                     &key
                     path
                     on-selected
                     on-no-match
                     on-ambiguous
                     on-invalid)
 "Resolve a kernel selector other than :OLD against LINES, a simple-vector of
line strings. PATH selects the language for :SYMBOL. Call ON-SELECTED with
1-based inclusive (START . END) ranges, ON-NO-MATCH with candidates,
ON-AMBIGUOUS with candidates, or ON-INVALID with a code and message."
 (declare (type function on-selected on-no-match on-ambiguous on-invalid))
 (aitools.kernel.domain:flow-call
  (resolve-line-selector/k lines
                           selector
                           :language
                           (and path (language-for-path path)))
  (:on-selected (ranges)
                (aitools.kernel.domain:flow-continue :on-selected ranges))
  (:on-no-match (candidates)
                (aitools.kernel.domain:flow-continue :on-no-match candidates))
  (:on-ambiguous (candidates)
                 (aitools.kernel.domain:flow-continue :on-ambiguous candidates))
  (:on-invalid (code message)
               (aitools.kernel.domain:flow-continue :on-invalid code message))))

(aitools.kernel.domain:define-flow
 (selection-error context
                  selection-kind
                  candidates
                  &key
                  path
                  code
                  message
                  on-error)
 "Report SELECTION-KIND (:NO-MATCH, :AMBIGUOUS, or :INVALID) through
ON-ERROR. No-match and ambiguous results include CANDIDATES; invalid results
use CODE and MESSAGE."
 (declare (type function on-error))
 (ecase selection-kind
   (:no-match
    (aitools.kernel.domain:flow-continue :on-error
                                         "selection.no-match"
                                         (format nil
                                                 "the selector matched nothing in ~A"
                                                 path)
                                         :candidates
                                         candidates
                                         :repairs
                                         (list
                                          (repair "read-file"
                                                  "Read the file to choose a selector."
                                                  (command-line context
                                                                (list "read"
                                                                      path))))))
   (:ambiguous
    (aitools.kernel.domain:flow-continue :on-error
                                         "selection.ambiguous"
                                         (format nil
                                                 "the selector matched ~D places in ~A; it must match one"
                                                 (length candidates)
                                                 path)
                                         :candidates
                                         candidates
                                         :repairs
                                         (list
                                          (repair "use-range"
                                                  "Select one match by its line range."
                                                  (command-line context
                                                                (list "read"
                                                                      path
                                                                      "--range"
                                                                      (format
                                                                       nil
                                                                       "~D:~D"
                                                                       (json-object-get
                                                                        (first
                                                                         candidates)
                                                                        "line")
                                                                       (json-object-get
                                                                        (first
                                                                         candidates)
                                                                        "end_line"))))))))
   (:invalid
    (aitools.kernel.domain:flow-continue :on-error
                                         code
                                         message
                                         :repairs
                                         (list
                                          (if (string= code
                                                       "input.unsupported-language")
                                              (repair "use-range"
                                                      "Select lines by number instead."
                                                      (command-line context
                                                                    (list "read"
                                                                          path
                                                                          "--range"
                                                                          "1:80")))
                                              (repair "fix-pattern"
                                                      "Correct the regular expression; see the syntax rules."
                                                      "aitools schema read")))))))
