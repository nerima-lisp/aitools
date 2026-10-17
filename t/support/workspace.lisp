;;;; t/support/workspace.lisp
(in-package #:aitools.test.support)

(defun %temporary-directory (prefix)
  (let* ((template (format nil "~A/~A-XXXXXX"
                           (string-right-trim "/"
                                               (or (sb-posix:getenv "TMPDIR") "/tmp"))
                           prefix))
         (created (sb-posix:mkdtemp template)))
    (string-right-trim "/"
                       (sb-ext:native-namestring
                        (truename (sb-ext:parse-native-namestring
                                   (concatenate 'string created "/")))))))

(defun call-with-workspace (function &key (prefix "aitools-test") chdir)
  "Call FUNCTION with fresh workspace and XDG state-home paths.
FUNCTION receives two native strings: ROOT and STATE-HOME. CHDIR makes ROOT
the current directory for the dynamic extent of FUNCTION. Both directories
are removed afterwards and the inherited XDG_STATE_HOME and cwd are restored."
  (let* ((base (%temporary-directory prefix))
         (root (concatenate 'string base "/work"))
         (state (concatenate 'string base "/state"))
         (previous-xdg (sb-posix:getenv "XDG_STATE_HOME"))
         (previous-cwd (sb-posix:getcwd)))
    (sb-posix:mkdir root #o755)
    (sb-posix:mkdir state #o755)
    (unwind-protect
         (progn
           (sb-posix:setenv "XDG_STATE_HOME" state 1)
           (when chdir (sb-posix:chdir root))
           (funcall function root state))
      (if previous-xdg
          (sb-posix:setenv "XDG_STATE_HOME" previous-xdg 1)
          (sb-posix:unsetenv "XDG_STATE_HOME"))
      (sb-posix:chdir previous-cwd)
      (uiop:delete-directory-tree
       (sb-ext:parse-native-namestring (concatenate 'string base "/"))
       :validate (lambda (path) (search (file-namestring (pathname base))
                                        (namestring path)))
       :if-does-not-exist :ignore))))

(defmacro with-workspace ((root state &key chdir) &body body)
  `(call-with-workspace (lambda (,root ,state) ,@body) :chdir ,chdir))
