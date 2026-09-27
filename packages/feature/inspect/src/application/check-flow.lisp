;;;; packages/feature/inspect/src/application/check-flow.lisp
;;;;
;;;; `check`: JSON syntax, or Lisp delimiter balance. A problem is
;;;; `input.syntax-error` with `diagnostics[{line,col,message}]`.
(in-package #:aitools.inspect.application)

(defun %join-lines (lines)
  (format nil "~{~A~^~%~}" (coerce lines 'list)))

(defun %check-problems (octets format dialect)
  "(line col message) lists for OCTETS checked as FORMAT, NIL when valid."
  (let ((lines (decode-text-lines octets)))
    (if (eq format :json)
        (json-diagnostics (%join-lines lines))
        (lisp-balance-diagnostics lines dialect))))

(defun %report-check (context path display format problems on-ok on-error)
  "DISPLAY is the reported path (root-relative or absolute); PATH is
the argument as typed, kept for the re-runnable repair command."
  (if (null problems)
      (funcall on-ok (list (cons "path" display)
                           (cons "valid" t)
                           (cons "format" (string-downcase (symbol-name format)))))
      (destructuring-bind (line col message) (first problems)
        (fail on-error "input.syntax-error"
              (format nil "~A is not valid ~A: ~A at line ~D, column ~D"
                      display (if (eq format :json) "JSON" "Lisp") message line col)
              :diagnostics (mapcar (lambda (problem)
                                     (json-object "line" (first problem) "col" (second problem)
                                                  "message" (third problem)))
                                   problems)
              :repairs (list (repair "read-around" "Read the lines around the first problem."
                                     (command-line context (list "read" path "--range"
                                                                 (format nil "~D:~D" (max 1 (- line 5)) (+ line 5))))))))))

(defun %check-target (context target path format dialect on-ok on-error)
  (record-read/k context target
                 :on-error on-error
                 :on-recorded (lambda ()
                                (multiple-value-bind (octets problem) (read-target-octets context target)
                                  (if (null octets)
                                      (fail-target-read context target on-error problem)
                                      (%report-check context path (target-display-path target) format
                                                     (%check-problems octets format dialect)
                                                     on-ok on-error))))))

(defun check-flow (ports path &key root tx lock-timeout format on-ok on-partial on-error)
  "`check`. Calls ON-OK (fields) or ON-ERROR."
  (declare (ignore on-partial) (type function on-ok on-error))
  (flet ((check-file (context target)
           (multiple-value-bind (detected dialect) (check-format-for-path (file-target-absolute target))
             (let ((format (cond ((null format) detected) ((string= format "json") :json) (t :lisp))))
               (if format
                   (%check-target context target path format (or dialect :common-lisp) on-ok on-error)
                   (fail on-error "input.unsupported-format"
                         (format nil "cannot tell the format of ~A from its name; check handles JSON and Lisp" path)
                         :repairs (list (repair "name-format" "Name the format explicitly."
                                                (command-line context (list "check" path "--format" "json"))))))))))
    (call-with-inspect-context/k
     ports :root root :tx tx :lock-timeout lock-timeout :on-error on-error
     :on-ready (lambda (context)
                 (call-with-readable-file/k context path
                                            :on-error on-error
                                            :on-file (lambda (target) (check-file context target)))))))
