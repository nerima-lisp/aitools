(in-package #:aitools.edit.test)
(describe "aitools archive extract symlink safety"
  (it "refuses the first archive symlink chain during planning without writes"
    (call-with-edit-workspace
     (lambda ()
       (put "symlink-a.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "x/d" :kind :symlink :link-target "..")
                   (aitools.text.domain:make-archive-member
                    :name "s" :kind :symlink :link-target "x/d/.."))))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "symlink-a.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))

  (it "checks unselected archive symlinks when extracting one selected entry"
    (call-with-edit-workspace
     (lambda ()
       (put "symlink-a-selected.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "x/d" :kind :symlink :link-target "..")
                   (aitools.text.domain:make-archive-member
                    :name "s" :kind :symlink :link-target "x/d/.."))))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "symlink-a-selected.tar")
                :to "dest" :entry (list "s"))
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))

  (it "refuses an archive symlink chain escaping the workspace during planning"
    (call-with-edit-workspace
     (lambda ()
       (put "symlink-b.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "a/b/c" :kind :symlink :link-target "../..")
                   (aitools.text.domain:make-archive-member
                    :name "s" :kind :symlink :link-target "a/b/c/../../../escaped-target"))))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "symlink-b.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))

  (it "refuses a file beneath an archive symlink during planning"
    (call-with-edit-workspace
     (lambda ()
       (put "symlink-c.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "a/b/c" :kind :symlink :link-target "../..")
                   (aitools.text.domain:make-archive-member
                    :name "s" :kind :symlink :link-target "a/b/c/../../../escaped-target")
                   (aitools.text.domain:make-archive-member
                    :name "s/PWNED" :kind :file :data (bytes "pwned")))))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "symlink-c.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))

  (it "refuses a file beneath an archive symlink even when its target stays inside"
    (call-with-edit-workspace
     (lambda ()
       (put "symlink-d.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "x/d" :kind :symlink :link-target "..")
                   (aitools.text.domain:make-archive-member
                    :name "x/d/f" :kind :file :data (bytes "payload")))))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "symlink-d.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))

  (it "refuses an archive symlink target that traverses a destination symlink"
    (call-with-edit-workspace
     (lambda ()
       (put "existing-target.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "s" :kind :symlink :link-target "x/d/.."))))
       (sb-posix:mkdir (disk "dest") #o755)
       (sb-posix:mkdir (disk "dest/x") #o755)
       (sb-posix:symlink ".." (disk "dest/x/d"))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "existing-target.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))

  (it "refuses an archive file beneath a destination symlink during planning"
    (call-with-edit-workspace
     (lambda ()
       (put "existing-parent.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "s/PWNED" :kind :file :data (bytes "pwned")))))
       (sb-posix:mkdir (disk "dest") #o755)
       (sb-posix:symlink "target" (disk "dest/s"))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "existing-parent.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before))))))
