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

(aitools.kernel.domain:define-flow
 (%report-check context path display format problems &key on-ok on-error)
 "Report whether the file at DISPLAY is valid, with re-runnable repairs."
 (declare (type function on-ok on-error))
 (if (null problems)
     (aitools.kernel.domain:flow-continue
      :on-ok
      (list (cons "path" display)
            (cons "valid" t)
            (cons "format" (string-downcase (symbol-name format)))))
     (destructuring-bind (line col message) (first problems)
       (aitools.kernel.domain:flow-continue
        :on-error
        "input.syntax-error"
        (format nil
                "~A is not valid ~A: ~A at line ~D, column ~D"
                display
                (if (eq format :json) "JSON" "Lisp")
                message
                line
                col)
        :diagnostics
        (mapcar (lambda (problem)
                  (json-object "line"
                               (first problem)
                               "col"
                               (second problem)
                               "message"
                               (third problem)))
                problems)
        :repairs
        (list
         (repair "read-around"
                 "Read the lines around the first problem."
                 (command-line context
                               (list "read"
                                     path
                                     "--range"
                                     (format nil "~D:~D"
                                             (max 1 (- line 5))
                                             (+ line 5))))))))))

(aitools.kernel.domain:define-flow
 (%check-target context target path format dialect &key on-ok on-error)
 (declare (type function on-ok on-error))
 (aitools.kernel.domain:flow-call
  (record-read/k context target)
  (:on-recorded ()
    (multiple-value-bind (octets problem)
        (read-target-octets context target)
      (if octets
          (aitools.kernel.domain:flow-call
           (%report-check context
                          path
                          (target-display-path target)
                          format
                          (%check-problems octets format dialect))
           (:on-ok (fields)
             (aitools.kernel.domain:flow-continue :on-ok fields))
           (:on-error (code message &key diagnostics repairs)
             (aitools.kernel.domain:flow-continue
              :on-error code message :diagnostics diagnostics :repairs repairs)))
          (aitools.kernel.domain:flow-call
           (%read-target-error/k context target problem)
           (:on-error (code message &key candidates repairs)
             (if candidates
                 (aitools.kernel.domain:flow-continue
                  :on-error code message :candidates candidates :repairs repairs)
                 (aitools.kernel.domain:flow-continue
                  :on-error code message :repairs repairs)))))))
  (:on-error (code message &key repairs)
    (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))

(aitools.kernel.domain:define-flow
 (check-flow ports
             path
             &key
             root
             tx
             lock-timeout
             format
             on-ok
             on-partial
             on-error)
 "The check command. Calls ON-OK (fields) or ON-ERROR."
 (declare (ignore on-partial)
          (type function on-ok on-error))
 (labels ((check-file (context target)
                      (multiple-value-bind (detected dialect)
                          (check-format-for-path (file-target-absolute target))
                        (let ((format
                               (cond
                                ((null format) detected)
                                ((string= format "json") :json)
                                (t :lisp))))
                          (if format
                              (aitools.kernel.domain:flow-call
                               (%check-target context
                                              target
                                              path
                                              format
                                              (or dialect :common-lisp))
                               (:on-ok (fields)
                                       (aitools.kernel.domain:flow-continue :on-ok fields))
                               (:on-error (code message &key candidates diagnostics conflicts repairs)
                                          (aitools.kernel.domain:flow-continue
                                           :on-error code message
                                           :candidates candidates
                                           :diagnostics diagnostics
                                           :conflicts conflicts
                                           :repairs repairs)))
                            (aitools.kernel.domain:flow-continue
                             :on-error
                             "input.unsupported-format"
                             (format nil
                                     (concatenate
                                      'string
                                      "cannot tell the format of ~A from its name; "
                                      "check handles JSON and Lisp")
                                     path)
                             :repairs
                             (list
                              (repair "name-format"
                                      "Name the format explicitly."
                                      (command-line context
                                                    (list "check"
                                                          path
                                                          "--format"
                                                          "json"))))))))))
   (aitools.kernel.domain:flow-call
    (inspect-context-flow ports :root root :tx tx :lock-timeout lock-timeout)
    (:on-ready (context)
               (aitools.kernel.domain:flow-call
                (call-with-readable-file/k context path)
                (:on-file (target)
                          (check-file context target))
                (:on-error (code message &key candidates diagnostics conflicts repairs)
                           (aitools.kernel.domain:flow-continue
                            :on-error code message
                            :candidates candidates
                            :diagnostics diagnostics
                            :conflicts conflicts
                            :repairs repairs))))
    (:on-error (code message &key repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message :repairs repairs)))))
