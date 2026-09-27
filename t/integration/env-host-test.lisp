;;;; t/integration/env-host-test.lisp
;;;;
;;;; The env flows over the production ports on the machine running the
;;;; suite: real uname(2), statfs(2), /proc or sysctl/vm_stat/ps/lsof, and the
;;;; installed zoneinfo database. Field presence is asserted on Linux and
;;;; Darwin alike; a check whose source the host lacks
;;;; (no zoneinfo, no lsof in a build sandbox) is skipped, not passed.
(in-package #:aitools.env.test)

(defun host-ports ()
  "Fresh production ports per use: construction does no I/O, and ports built
at load time would hold the adapter functions compiled then."
  (aitools.env.infrastructure:make-production-env-ports))

(defun host-zoneinfo-file (name)
  (find-if #'probe-file
           (mapcar (lambda (directory) (join-directory directory name))
                   (zoneinfo-directories (uiop:getenv "TZDIR")))))

(describe "aitools.env integration on this host"
  (it "fills sys info from the real host"
    (multiple-value-bind (kind fields) (run-flow #'sys-info/k (host-ports))
      (expect kind :to-be :ok)
      (expect (field fields "os") :to-equal (string-downcase (software-type)))
      (expect (plusp (length (field fields "arch"))) :to-be-truthy)
      (expect (plusp (field fields "cpus")) :to-be-truthy)
      (expect (field fields "uid") :to-be (sb-posix:getuid))
      (let ((memory (field fields "memory")) (disk (field fields "disk")))
        (expect (< 0 (object-field memory "available") (object-field memory "total")) :to-be-truthy)
        (expect (<= 0 (object-field disk "available") (object-field disk "total")) :to-be-truthy)
        (expect (plusp (object-field disk "total")) :to-be-truthy))))

  (it "lists this test process in sys procs without touching it"
    (multiple-value-bind (kind fields) (run-flow #'sys-procs/k (host-ports) :limit 100000)
      (expect kind :to-be :ok)
      (let ((self (find (sb-posix:getpid) (field fields "items") :key (lambda (item) (object-field item "pid")))))
        (expect self :to-be-truthy)
        (expect (object-field self "ppid") :to-be (sb-posix:getppid))
        (expect (stringp (object-field self "started")) :to-be-truthy))))

  (it "shows a socket this test is listening on, or skips when no source"
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
             (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
             (sb-bsd-sockets:socket-listen socket 1)
             (multiple-value-bind (address port) (sb-bsd-sockets:socket-name socket)
               (declare (ignore address))
               (multiple-value-bind (kind result) (run-flow #'sys-ports/k (host-ports))
                 ;; No /proc/net/tcp on Linux or no lsof on Darwin (a build
                 ;; sandbox) is a missing source: skip, never a hard failure.
                 (unless (eq kind :ok)
                   (expect (first result) :to-equal "environment.unavailable")
                   (cl-weave:skip "no listening-socket source on this host"))
                 (expect (field result "total") :to-be (length (field result "items")))
                 (expect (find port (field result "items")
                               :key (lambda (item) (object-field item "port")))
                         :to-be-truthy))))
        (sb-bsd-sockets:socket-close socket))))

  (it "reports path null for a command that is not installed"
    (multiple-value-bind (kind fields) (run-flow #'sys-tools/k (host-ports) :names '("aitools-no-such-command-9f3"))
      (expect kind :to-be :ok)
      (expect (json-null-p (object-field (first (field fields "items")) "path")) :to-be-truthy)))

  (it "reads the installed America/New_York exactly like the captured fixture"
    (let ((path (host-zoneinfo-file "America/New_York")))
      (unless path (cl-weave:skip "no zoneinfo database on this host"))
      (let ((host-zone (parse-tzif "America/New_York" (octets-from-latin-1 (uiop:read-file-string path :external-format :latin-1)))))
        (dolist (instant (list (utc-ms 2026 3 8 6 59 59) (utc-ms 2026 3 8 7 0 0)
                               (utc-ms 2026 11 1 6 0 0) (utc-ms 2040 7 1 0 0 0)))
          (expect (offset-at host-zone instant) :to-equal (offset-at *new-york* instant))))))

  (it "converts through the installed database for Asia/Tokyo"
    (unless (host-zoneinfo-file "Asia/Tokyo") (cl-weave:skip "no zoneinfo database on this host"))
    (multiple-value-bind (kind fields) (run-flow #'time-convert/k (host-ports) "2026-03-08T06:30:00Z" :tz "Asia/Tokyo")
      (expect kind :to-be :ok)
      (expect (field fields "result") :to-equal "2026-03-08T15:30:00+09:00"))))

(defun call-in-directory (directory thunk)
  "Call THUNK with the process working directory set to DIRECTORY."
  (let ((previous (uiop:getcwd)))
    (unwind-protect (progn (uiop:chdir directory) (funcall thunk))
      (uiop:chdir previous))))

(defun host-workspace-root ()
  (funcall (aitools.env.application::env-ports-workspace-root (host-ports))))

(defun git-ancestor-p (directory)
  (loop for current = (uiop:ensure-directory-pathname directory) then (uiop:pathname-parent-directory-pathname current)
        thereis (probe-file (merge-pathnames ".git" current))
        until (equal (namestring current) "/")))

(defun call-with-scratch-directory (function)
  "Call FUNCTION with a fresh directory's real namestring (trailing slash),
made under the first of $TMPDIR, /tmp and /var/tmp with no .git above it,
else under $TMPDIR; removed afterwards."
  (let* ((candidates (remove nil (list (uiop:getenv "TMPDIR") "/tmp" "/var/tmp")))
         (parent (or (find-if-not #'git-ancestor-p candidates) (first candidates)))
         (base (namestring (truename (uiop:ensure-directory-pathname
                                      (sb-posix:mkdtemp (format nil "~A/aitools-env-root-XXXXXX"
                                                                (string-right-trim "/" parent))))))))
    (unwind-protect (funcall function base)
      (uiop:delete-directory-tree (pathname base)
                                  :validate (lambda (path) (search "aitools-env-root-" (namestring path)))))))

(describe "aitools.env production workspace root without --root"
  (it "is the nearest ancestor holding .git, file or directory"
    (call-with-scratch-directory
     (lambda (base)
       (let ((nested (concatenate 'string base "repo/a/b/")))
         (ensure-directories-exist nested)
         (ensure-directories-exist (concatenate 'string base "repo/.git/"))
         (expect (call-in-directory nested #'host-workspace-root) :to-equal (concatenate 'string base "repo/"))
         ;; A linked worktree's .git is a file; it marks the root just the same.
         (with-open-file (out (concatenate 'string base "repo/a/.git") :direction :output)
           (write-line "gitdir: elsewhere" out))
         (expect (call-in-directory nested #'host-workspace-root) :to-equal (concatenate 'string base "repo/a/"))))))

  (it "is the working directory itself when no ancestor holds .git"
    (call-with-scratch-directory
     (lambda (base)
       (let ((nested (concatenate 'string base "plain/dir/")))
         (ensure-directories-exist nested)
         (when (git-ancestor-p nested)
           (cl-weave:skip "every temporary directory is inside a git checkout"))
         (expect (call-in-directory nested #'host-workspace-root) :to-equal nested))))))

(defun port (accessor &rest arguments)
  "Call the production port slot ACCESSOR (a symbol in AITOOLS.ENV.APPLICATION)."
  (apply (funcall (find-symbol (format nil "ENV-PORTS-~A" accessor) '#:aitools.env.application) (host-ports))
         arguments))

(defun write-scratch-file (path content &key (mode #o644))
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string content out))
  (sb-posix:chmod path mode)
  path)

(describe "aitools.env production adapters"
  (it "reads a file as UTF-8 or Latin-1, and answers NIL past the read bound or for a missing file"
    (call-with-scratch-directory
     (lambda (base)
       (let ((path (write-scratch-file (concatenate 'string base "t.txt") "héllo")))
         (expect (port "READ-FILE" path :utf-8) :to-equal "héllo")
         (expect (port "READ-FILE" path :latin-1) :to-equal (map 'string #'code-char #(104 195 169 108 108 111)))
         (let ((aitools.env.infrastructure::*maximum-read-bytes* 3))
           (expect (port "READ-FILE" path :utf-8) :to-be nil))
         (expect (port "READ-FILE" (concatenate 'string base "missing.txt") :utf-8) :to-be nil)))))

  (it "lists a directory without . and .., and answers NIL for one that does not exist"
    (call-with-scratch-directory
     (lambda (base)
       (write-scratch-file (concatenate 'string base "a") "")
       (ensure-directories-exist (concatenate 'string base "d/"))
       (expect (sort (port "LIST-DIRECTORY" base) #'string<) :to-equal '("a" "d"))
       (expect (port "LIST-DIRECTORY" (concatenate 'string base "none")) :to-be nil))))

  (it "reads a symlink's target and answers NIL for a path that is no link"
    (call-with-scratch-directory
     (lambda (base)
       (sb-posix:symlink "target/x" (concatenate 'string base "link"))
       (expect (port "READ-LINK" (concatenate 'string base "link")) :to-equal "target/x")
       (expect (port "READ-LINK" base) :to-be nil))))

  (it "counts only executable regular files as executable"
    (call-with-scratch-directory
     (lambda (base)
       (let ((script (write-scratch-file (concatenate 'string base "run") "#!/bin/sh" :mode #o755))
             (plain (write-scratch-file (concatenate 'string base "plain") "" :mode #o644)))
         (expect (port "EXECUTABLE-P" script) :to-be-truthy)
         (expect (port "EXECUTABLE-P" plain) :to-be nil)
         (expect (port "EXECUTABLE-P" base) :to-be nil)
         (expect (port "EXECUTABLE-P" (concatenate 'string base "none")) :to-be nil)))))

  (it "runs a program to its exit, stops one at the timeout, and reports one that cannot start"
    (let ((sh (aitools.env.application:find-executable (host-ports) "sh"))
          (sleep (aitools.env.application:find-executable (host-ports) "sleep")))
      (unless (and sh sleep) (cl-weave:skip "sh or sleep is not on PATH"))
      (expect (multiple-value-list (port "RUN-PROGRAM" sh '("-c" "printf out; printf err >&2; exit 3") 10))
              :to-equal '(:exited 3 "out" "err"))
      (expect (nth-value 0 (port "RUN-PROGRAM" sleep '("5") 1/5)) :to-be :timeout)
      (expect (multiple-value-list (port "RUN-PROGRAM" "/nonexistent/aitools-no-such-program" '() 1))
              :to-equal '(:not-started nil nil nil))))

  (it "answers NIL for the space of a path that does not exist"
    (expect (port "FILE-SYSTEM-SPACE" "/nonexistent/aitools-no-such-directory") :to-be nil)))
