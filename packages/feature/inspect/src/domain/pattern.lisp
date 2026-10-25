;;;;
;;;; Regular expressions as the inspect context uses them: compiled
;;;; once with cl-regex-kit and matched against one line string at a time
;;;; (selectors, `--where ~`). A syntax error is reported as data so the
;;;; caller can answer `input.syntax-error`.
(in-package #:aitools.inspect.domain)

(aitools.kernel.domain:define-flow
 (compile-pattern/k pattern &key ignore-case on-compiled on-error)
 "Compile PATTERN and call ON-COMPILED with the regex or ON-ERROR with a syntax message."
 (declare (type function on-compiled on-error))
 (handler-case (aitools.kernel.domain:flow-continue :on-compiled
                                                    (cl-regex-kit:compile-regex
                                                     pattern
                                                     :case-insensitive
                                                     (and ignore-case t)))
   (cl-regex-kit:regex-syntax-error (condition)
     (aitools.kernel.domain:flow-continue :on-error
                                          (format nil
                                                  "invalid regular expression ~S at offset ~D: ~A"
                                                  pattern
                                                  (cl-regex-kit:regex-syntax-error-position
                                                   condition)
                                                  (cl-regex-kit:regex-syntax-error-reason
                                                   condition))))))

(aitools.kernel.domain:define-flow (call-with-regex-limit/k thunk &key on-limit)
                                   "Run THUNK and call ON-LIMIT with a message if the bounded regex
executor exhausts its step budget."
                                   (declare (type function thunk on-limit))
                                   (handler-case (funcall thunk)
                                     (cl-regex-kit:advanced-regex-limit-error (condition)
                                       (aitools.kernel.domain:flow-continue
                                        :on-limit
                                        (princ-to-string condition)))))

(defun pattern-matches-p (regex text)
  (and (cl-regex-kit:scan regex text) t))

(defun pattern-group-string (regex text group-name)
  "The text captured by GROUP-NAME in the first match of REGEX in TEXT, or
NIL when there is no match or the group did not participate."
  (let ((match (cl-regex-kit:scan regex text)))
    (and match
         (cl-regex-kit:match-group-string match
                                          (cl-regex-kit:regex-group-index regex
                                                                          group-name)
                                          text))))
