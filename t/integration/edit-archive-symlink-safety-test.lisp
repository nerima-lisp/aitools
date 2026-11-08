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
  (it "refuses the repeated archive symlink alias chain during planning without writes"
    (call-with-edit-workspace
     (lambda ()
       (put "symlink-f1-alias.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "x/d" :kind :symlink :link-target "..")
                   (aitools.text.domain:make-archive-member
                    :name "s" :kind :symlink
                    :link-target "x/d/x/d/../../escaped-target"))))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "symlink-f1-alias.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))
  (it "rejects a destination symlink outside the workspace without writes"
    (call-with-edit-workspace
     (lambda ()
       (put "destination-outside.tar"
            (aitools.text.domain:write-tar (list (member-file "created.txt" "payload"))))
       (sb-posix:symlink ".." (disk "dest"))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "destination-outside.tar") :to "dest")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)
         (expect (kind "dest/created.txt") :to-be :absent)))))
  (it "resolves an in-workspace destination symlink before extraction"
    (call-with-edit-workspace
     (lambda ()
       (put "destination-inside.tar"
            (aitools.text.domain:write-tar (list (member-file "created.txt" "payload"))))
       (sb-posix:mkdir (disk "real-dest") #o755)
       (sb-posix:symlink "real-dest" (disk "dest"))
       (expect (nth-value 0 (run "archive.extract" (list "destination-inside.tar") :to "dest"))
               :to-be :ok)
       (expect (text "real-dest/created.txt") :to-equal "payload")
       (expect (kind "dest/created.txt") :to-be :file))))

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
         (expect-unchanged before)))))
  (it "refuses links through archive symlinks before trailing .. with --to . and no writes"
  (dolist (links
           (list (list (list "D" ".") (list "s" "D/.."))
                 (list (list "X/D" "..") (list "s" "X/D/.."))
                 (list (list "X/D" "..") (list "s" "x/d/.."))
                 (list (list "é" ".") (list "s" "é/.."))
                 (list (list "ß" ".") (list "s" "SS/.."))
                 (list (list "İ" ".") (list "s" (format nil "i~C/.." (code-char #x307))))
                 (list (list "A" "B") (list "B" ".") (list "s" "A/.."))
                 (list (list "a/B" "..") (list "s" "a/B/.."))))
    (call-with-edit-workspace
     (lambda ()
       (put "trailing-link.tar"
            (aitools.text.domain:write-tar
             (mapcar (lambda (link)
                       (aitools.text.domain:make-archive-member
                        :name (first link) :kind :symlink :link-target (second link)))
                     links)))
       (let ((before (snapshot)))
         (with-error (code)
           (run "archive.extract" (list "trailing-link.tar") :to ".")
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before))))))
  (it "refuses a target whose final component is an existing destination symlink without writes"
  (call-with-edit-workspace
   (lambda ()
     (put "final-destination-link.tar"
          (aitools.text.domain:write-tar
           (list (aitools.text.domain:make-archive-member
                  :name "y" :kind :symlink :link-target "e"))))
     (sb-posix:mkdir (disk "dest") #o755)
     (sb-posix:symlink ".." (disk "dest/e"))
     (let ((before (snapshot)))
       (with-error (code)
         (run "archive.extract" (list "final-destination-link.tar") :to "dest")
         (expect code :to-equal "refusal.outside-workspace"))
       (expect-unchanged before)))))
  #+linux
(it "extracts README and readme as distinct entries on a case-sensitive filesystem"
  (call-with-edit-workspace
   (lambda ()
     (put "case-distinct.tar"
          (aitools.text.domain:write-tar
           (list (member-file "README" "upper")
                 (member-file "readme" "lower"))))
     (expect (nth-value 0 (run "archive.extract" (list "case-distinct.tar") :to ".")) :to-be :ok)
     (expect (text "README") :to-equal "upper")
     (expect (text "readme") :to-equal "lower"))))
  #+darwin
(it "handles case-only archive name collisions at commit on macOS"
  (call-with-edit-workspace
   (lambda ()
     (put "case-distinct.tar"
          (aitools.text.domain:write-tar
           (list (member-file "README" "upper")
                 (member-file "readme" "lower"))))
     (let ((before (snapshot)))
       (multiple-value-bind (kind code fields)
           (run "archive.extract" (list "case-distinct.tar") :to ".")
         (declare (ignore fields))
         (case kind
           (:ok
            (expect (text "README") :to-equal "upper")
            (expect (text "readme") :to-equal "lower"))
           (:error
            (expect code :to-equal "refusal.exists")
            (expect-unchanged before))
           (otherwise (expect kind :to-be :ok)))))))))
