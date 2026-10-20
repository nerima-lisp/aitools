(in-package #:aitools.env.application)

(defun find-executable (ports name)
  "The first PATH directory entry NAME that is an executable file, or NIL."
  (let ((path (%getenv ports "PATH")))
    (when path
      (dolist (directory (aitools.env.domain:split-search-path path))
        (let ((candidate (aitools.env.domain:join-directory directory name)))
          (when (funcall (env-ports-executable-p ports) candidate)
            (return candidate)))))))

(defun %tool-version (ports path arguments timeout-seconds)
  "The first nonblank line of PATH's version output (stdout, else stderr:
some tools print their version there), or NIL when it did not finish in
TIMEOUT-SECONDS or printed nothing."
  (multiple-value-bind (status exit-code stdout stderr) (%run ports path arguments timeout-seconds)
    (declare (ignore exit-code))
    (when (eq status :exited)
      (or (aitools.env.domain:first-output-line (or stdout ""))
          (aitools.env.domain:first-output-line (or stderr ""))))))

(aitools.kernel.domain:define-flow
    (sys-tools/k ports &key names (timeout "5s") on-ok on-error)
  "NAMES NIL means the default tool list. TIMEOUT bounds each version probe."
  (let* ((bad (find-if-not #'aitools.env.domain:valid-tool-name-p names))
         (timeout-milliseconds
           (handler-case
               (aitools.kernel.domain:duration-milliseconds
                (aitools.kernel.domain:parse-duration timeout))
             (aitools.kernel.domain:invalid-duration-error () nil))))
    (cond
      (bad
       (aitools.kernel.domain:flow-continue
        :on-error
        "argument.invalid"
        (format nil "~S is not a bare command name" bad)
        :repairs
        (list (repair "use-name"
                      "Pass command names without a directory."
                      "aitools sys tools git"))))
      ((null timeout-milliseconds)
       (aitools.kernel.domain:flow-continue
        :on-error
        "argument.invalid"
        (format nil "--timeout: not a duration: ~S" timeout)
        :repairs
        (list (repair "fix-duration"
                      "Durations are <number>ms|s|m|h|d."
                      "aitools sys tools --timeout 5s"))))
      (t
       (let ((probes
               (if names
                   (mapcar
                    (lambda (name)
                      (cons name
                            (or (cdr (assoc name aitools.data:*env-default-tools*
                                            :test #'string=))
                                aitools.data:*env-version-arguments-default*)))
                    names)
                   aitools.data:*env-default-tools*))
             (items nil))
         (dolist (probe probes)
           (destructuring-bind (name . arguments) probe
             (let ((path (find-executable ports name)))
               (push (%object
                      (cons "name" name)
                      (cons "path" (%null-if-nil path))
                      (cons "version"
                            (%null-if-nil
                             (and path
                                  (%tool-version
                                   ports path arguments
                                   (/ timeout-milliseconds 1000))))))
                     items))))
         (aitools.kernel.domain:flow-continue
          :on-ok
          (list (cons "items" (nreverse items))
                (cons "total" (length probes)))))))))
