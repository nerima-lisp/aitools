(in-package #:aitools.edit.test)

(describe "Org agenda and query refusals over several files"
  (it "refuses a missing path before reading any file"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "a.org" (%multi-scheduled "Alpha"))
         (multiple-value-bind (code message)
             (%multi-agenda-error boundaries :paths '("a.org" "missing.org"))
           (expect code :to-equal "input.not-found")
           (expect message :to-equal "missing.org does not exist"))
         (expect (%multi-query-error boundaries :paths '("nowhere"))
                 :to-equal "input.not-found")))))
  (it "refuses a path whose symlinks cannot be resolved"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "a.org" (%multi-scheduled "Alpha"))
         (sb-posix:symlink "loop.org" (disk "loop.org"))
         (multiple-value-bind (code message)
             (%multi-agenda-error boundaries :paths '("a.org" "loop.org"))
           (expect code :to-equal "input.not-found")
           (expect message :to-equal "cannot resolve loop.org"))))))
  (it "refuses a path that resolves outside the workspace"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries))
             (outside (concatenate 'string *home* "/outside.org")))
         (put "a.org" (%multi-scheduled "Alpha"))
         (with-open-file (out (sb-ext:parse-native-namestring outside)
                              :direction :output :if-exists :supersede)
           (write-string (%multi-scheduled "Outside") out))
         (multiple-value-bind (code message)
             (%multi-agenda-error boundaries :paths (list "a.org" outside))
           (expect code :to-equal "refusal.outside-workspace")
           (expect message :to-equal
                   (format nil "~A is outside the workspace" outside)))
         (sb-posix:symlink outside (disk "escape.org"))
         (expect (%multi-agenda-error boundaries :paths '("escape.org"))
                 :to-equal "refusal.outside-workspace")))))
  (it "refuses a path that is neither a regular file nor a directory"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (sb-posix:mkfifo (disk "pipe.org") #o600)
         (multiple-value-bind (code message)
             (%multi-agenda-error boundaries :paths '("pipe.org"))
           (expect code :to-equal "refusal.not-a-file")
           (expect message :to-equal
                   "pipe.org is not a regular file or directory"))))))
  (it "reports a path whose entry vanishes between resolution and stat"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "ghost.org" (%multi-scheduled "Ghost"))
         (cl-weave:with-mocked-functions
             (((symbol-function (quote aitools.workspace.application:host-stat))
               (let ((original (symbol-function (quote aitools.workspace.application:host-stat))))
                 (lambda (host path)
                   (unless (search "ghost.org" path)
                     (funcall original host path))))))
           (multiple-value-bind (code message)
               (%multi-agenda-error boundaries :paths '("ghost.org"))
             (expect code :to-equal "input.not-found")
             (expect message :to-equal "ghost.org does not exist")))))))
  (it "reports a directory that cannot be read"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "dir/a.org" (%multi-scheduled "Alpha"))
         (put "dir/locked/hidden.org" (%multi-scheduled "Hidden"))
         (sb-posix:chmod (disk "dir/locked") #o000)
         (unwind-protect
              (multiple-value-bind (code message)
                  (%multi-agenda-error boundaries :paths '("dir"))
                (expect code :to-equal "environment.io")
                (expect message :to-equal "cannot read directory dir/locked"))
           (sb-posix:chmod (disk "dir/locked") #o755))))))
  (it "maps a scan that loses its directory to the matching refusal"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "dir/a.org" (%multi-scheduled "Alpha"))
         (flet ((scan-failing-with (reason)
                  (cl-weave:with-mocked-functions
                      (((symbol-function (quote aitools.workspace.application:call-with-workspace-scan/k))
                        (lambda (host root &key on-error &allow-other-keys)
                          (declare (ignore host root))
                          (funcall on-error reason "dir"))))
                    (multiple-value-list
                     (%multi-agenda-error boundaries :paths '("dir"))))))
           (expect (scan-failing-with :not-found)
                   :to-equal '("input.not-found" "dir does not exist"))
           (expect (scan-failing-with :outside-root)
                   :to-equal '("refusal.outside-workspace"
                               "dir is outside the workspace")))))))
  (it "prefixes a file's parse failure with its path and stops at it"
    (call-with-edit-workspace
     (lambda ()
       (let* ((real-parser (cl-boundary-kit:boundary-context-require
                            (make-org-write-flow-boundaries) :org-parse))
              (boundaries
                (%multi-boundaries-with
                 :org-parse (lambda (octets &key source-name)
                              (if (search "BAD" (sb-ext:octets-to-string octets :external-format :utf-8))
                                  (values nil (aitools.org.domain:make-org-failure
                                               :kind :syntax :message "unterminated block"
                                               :line 2 :column 5))
                                  (funcall real-parser octets :source-name source-name))))))
         (put "a.org" (%multi-scheduled "Alpha"))
         (put "b.org" (format nil "* BAD~%"))
         (put "c.org" (%multi-scheduled "Gamma"))
         (multiple-value-bind (kind fields)
             (run-org-flow #'aitools.org.application:org-agenda/k boundaries
                           :paths '("c.org" "b.org" "a.org") :from "2026-10-19"
                           :through "2026-10-25" :future-repeats "all"
                           :at "2026-10-19T09:00:00" :tz "UTC"
                           :dst-policy "compatible" :root *root*)
           (expect kind :to-be :error)
           (expect (getf fields :code) :to-equal "input.syntax-error")
           (expect (getf fields :message) :to-equal "b.org: unterminated block"))
         (multiple-value-bind (code message)
             (%multi-query-error boundaries :paths '("a.org" "b.org"))
           (expect code :to-equal "input.syntax-error")
           (expect message :to-equal "b.org: unterminated block"))))))
  (it "reports a binary file among several without reading the rest"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "a.org" (%multi-scheduled "Alpha"))
         (put "b.org" (concatenate 'vector #(42 32) #(0)))
         (multiple-value-bind (code message)
             (%multi-agenda-error boundaries :paths '("a.org" "b.org"))
           (expect code :to-equal "input.unsupported-format")
           (expect message :to-equal "b.org is a binary file, not a UTF-8 Org document"))))))
  (it "rejects invalid options once, without naming a file"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "a.org" (%multi-scheduled "Alpha"))
         (put "b.org" (%multi-scheduled "Beta"))
         (multiple-value-bind (code message)
             (%multi-agenda-error boundaries :paths '("a.org" "b.org")
                                             :from "not-a-date")
           (expect code :to-equal "argument.invalid")
           (expect (search "a.org" message) :to-be nil)
           (expect (search "b.org" message) :to-be nil))))))
  (it "reports a parser that rejects the empty probe document"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries
               (%multi-boundaries-with
                :org-parse (lambda (octets &key source-name)
                             (declare (ignore octets source-name))
                             (values nil (%multi-failure "parser rejected input"))))))
         (put "a.org" (%multi-scheduled "Alpha"))
         (multiple-value-bind (code message)
             (%multi-agenda-error boundaries :paths '("a.org"))
           (expect code :to-equal "argument.invalid")
           (expect message :to-equal "parser rejected input"))))))
  (it "prefixes a per-file result failure with the file's path"
    (call-with-edit-workspace
     (lambda ()
       (let* ((calls 0)
              (boundaries
                (%multi-boundaries-with
                 :org-query (lambda (document match at tz dst-policy)
                              (declare (ignore document match at tz dst-policy))
                              (if (< (incf calls) 3)
                                  (values nil nil)
                                  (values nil (%multi-failure "query rejected")))))))
         (put "a.org" (%multi-scheduled "Alpha"))
         (put "b.org" (%multi-scheduled "Beta"))
         (multiple-value-bind (code message)
             (%multi-query-error boundaries :paths '("a.org" "b.org"))
           (expect code :to-equal "argument.invalid")
           (expect message :to-equal "b.org: query rejected"))
         (expect calls :to-be 3)))))
  (it "requires the agenda and query capabilities of the context"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "a.org" (%multi-scheduled "Alpha"))
         (expect (%multi-agenda-error
                  (cl-boundary-kit:boundary-context-with boundaries :org-agenda nil)
                  :paths '("a.org"))
                 :to-equal "internal.unexpected")
         (expect (%multi-query-error
                  (cl-boundary-kit:boundary-context-with boundaries :org-query nil)
                  :paths '("a.org"))
                 :to-equal "internal.unexpected")))))
  (it "reports an unresolved root for a multi-file read"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries))
             (missing (concatenate 'string *root* "/no-such-root")))
         (expect (multiple-value-list
                  (%multi-error
                   #'aitools.org.application:org-agenda/k
                   (list :paths '("a.org" "b.org") :from "2026-10-19"
                         :through "2026-10-19" :future-repeats "all"
                         :at "2026-10-19T09:00:00" :tz "UTC"
                         :dst-policy "compatible" :root missing)
                   boundaries))
                 :to-equal
                 (list "input.not-found"
                       (format nil "workspace root ~A does not exist" missing)))))))
  (it "names a path relative to the workspace, or absolute when it lies outside"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries))
             (missing (concatenate 'string *root* "/no-such-root")))
         (expect (aitools.org.application::%org-workspace-path
                  boundaries *root* "dir/a.org")
                 :to-equal "dir/a.org")
         (expect (aitools.org.application::%org-workspace-path
                  boundaries *root* (disk "dir/a.org"))
                 :to-equal "dir/a.org")
         (expect (aitools.org.application::%org-workspace-path
                  boundaries missing "dir/a.org")
                 :to-equal (aitools.workspace.application:user-path-absolute
                            (cl-boundary-kit:boundary-context-require
                             boundaries :org-workspace-host)
                            "dir/a.org")))))))
