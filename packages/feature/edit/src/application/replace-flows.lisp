;;;; packages/feature/edit/src/application/replace-flows.lisp
;;;;
;;;; `replace`: the multi-file write that validates every file
;;;; before the store writes any. Its file scan is in scan.lisp, and `apply`
;;;; in apply-flows.lisp.
(in-package #:aitools.edit.application)

;;; ---------------------------------------------------------------- replace

(defun compile-pattern/k (pattern fail on-regex &key fixed word ignore-case)
  "PATTERN compiled, or FAIL input.syntax-error."
  (declare (type function fail on-regex))
  (compile-search-pattern/k pattern :fixed fixed :word word :ignore-case ignore-case
                                     :on-regex on-regex
                                     :on-invalid (lambda (message) (funcall fail "input.syntax-error" message))))

(defun %replacement-function/k (replacement regex literal fail on-function)
  "The regex replacement function for REPLACEMENT: verbatim when LITERAL,
else the parsed replacement template. `\\N` naming an existing group is refused
(ON-FAILURE via FAIL with the repair rewritten to ${N})."
  (declare (type function fail on-function))
  (if literal
      (funcall on-function (literal-replacement replacement))
      (let ((perl (perl-backreferences replacement (regex-group-count regex))))
        (if perl
            (funcall fail "argument.invalid"
                     (format nil "the replacement writes \\~D, which cl-regex-kit copies literally; write ${~D} (or pass --literal-replacement for a literal backslash)"
                             (first perl) (first perl))
                     :rewrite (rewrite-perl-backreferences replacement))
            (let ((parts (handler-case (parse-replacement-template replacement)
                           (edit-refusal (condition)
                             (return-from %replacement-function/k
                               (funcall fail (edit-refusal-code condition) (edit-refusal-detail condition)))))))
              (funcall on-function (template-regex-replacement parts)))))))

