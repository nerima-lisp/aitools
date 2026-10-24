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

(defun call-with-workspace (function &key (prefix "aitools-test") chdir (initialize-state-home t))
  "Call FUNCTION with fresh workspace and state-home paths. FUNCTION receives
native strings ROOT and STATE-HOME. CHDIR makes ROOT the current directory
while FUNCTION runs. When INITIALIZE-STATE-HOME is true, STATE-HOME is created
and XDG_STATE_HOME is set to it; the previous value and cwd are restored
afterwards. The temporary tree is removed on exit."
  (let* ((base (%temporary-directory prefix))
         (root (concatenate 'string base "/work"))
         (state (concatenate 'string base "/state"))
         (previous-xdg (sb-posix:getenv "XDG_STATE_HOME"))
         (previous-cwd (sb-posix:getcwd)))
    (sb-posix:mkdir root #o755)
    (when initialize-state-home (sb-posix:mkdir state #o755))
    (unwind-protect
         (progn
           (when initialize-state-home (sb-posix:setenv "XDG_STATE_HOME" state 1))
           (when chdir (sb-posix:chdir root))
           (funcall function root state))
      (when initialize-state-home
        (if previous-xdg
            (sb-posix:setenv "XDG_STATE_HOME" previous-xdg 1)
            (sb-posix:unsetenv "XDG_STATE_HOME")))
      (sb-posix:chdir previous-cwd)
      (uiop:delete-directory-tree
       (sb-ext:parse-native-namestring (concatenate 'string base "/"))
       :validate (lambda (path) (search (file-namestring (pathname base))
                                        (namestring path)))
       :if-does-not-exist :ignore))))

(defmacro with-workspace (bindings &body body)
  (if (null bindings)
      (multiple-value-bind (adapter status) (find-symbol "CALL-WITH-WORKSPACE" *package*)
        (unless (and adapter (eq status :internal))
          (error "WITH-WORKSPACE without bindings requires a package-local CALL-WITH-WORKSPACE adapter"))
        `(,adapter (lambda () ,@body)))
      (destructuring-bind (root state &key chdir) bindings
        `(call-with-workspace (lambda (,root ,state) ,@body) :chdir ,chdir))))

(defun call-with-temporary-directory (function &key (prefix "aitools-test"))
  "Call FUNCTION with a fresh directory pathname and remove it afterwards."
  (let* ((base (%temporary-directory prefix))
         (directory (sb-ext:parse-native-namestring (format nil "~A/" base))))
    (unwind-protect
         (funcall function directory)
      (uiop:delete-directory-tree
       directory
       :validate (lambda (path)
                   (search (file-namestring (pathname base)) (namestring path)))
       :if-does-not-exist :ignore))))

(defun call-with-process-workspace (function)
  "Call FUNCTION with fresh workspace and state directory pathnames."
  (call-with-temporary-directory
   (lambda (directory)
     (let ((root (merge-pathnames "ws/" directory))
           (state (merge-pathnames "state/" directory)))
       (ensure-directories-exist root)
       (ensure-directories-exist (merge-pathnames "tmp/" state))
       (funcall function root state)))
   :prefix "aitools-process"))

(defun call-with-scratch-directory (function)
  "Call FUNCTION with the real path (symlinks resolved, no trailing `/`) of a fresh private directory, removing it afterwards."
  (call-with-temporary-directory
   (lambda (directory)
     (funcall function
              (string-right-trim "/" (sb-ext:native-namestring directory))))
   :prefix "aitools-it"))
