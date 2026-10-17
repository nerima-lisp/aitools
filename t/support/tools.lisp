;;;; t/support/tools.lisp
(in-package #:aitools.test.support)

(defun tool-path (name)
  "Return the executable named NAME on PATH, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                             :separator ":")
        for candidate = (and (plusp (length directory))
                             (merge-pathnames name
                                              (uiop:ensure-directory-pathname directory)))
        when (and candidate (probe-file candidate)
                   (not (uiop:directory-pathname-p candidate)))
          return candidate))

(defun skip-unless-tools (&rest names)
  "Skip the current cl-weave spec when any named executable is unavailable."
  (let ((missing (remove-if #'tool-path names)))
    (when missing
      (cl-weave:skip (format nil "required tool(s) not on PATH: ~{~A~^, ~}" missing)))
    (null missing)))
