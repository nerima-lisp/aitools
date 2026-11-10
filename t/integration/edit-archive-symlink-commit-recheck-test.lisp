(in-package #:aitools.edit.test)

(describe "archive extraction rechecks destination symlinks before commit"
  (it "refuses a parent symlink created after validation without committing outputs"
    (call-with-edit-workspace
     (lambda ()
       (put "parent-swap.tar"
            (aitools.text.domain:write-tar
             (list (member-file "payload" "payload")
                   (aitools.text.domain:make-archive-member
                    :name "link" :kind :symlink :link-target "sub/file"))))
       (let ((injected nil))
         (unwind-protect
              (let ((aitools.store.application::*fault-hook*
                      (lambda (point &rest details)
                        (declare (ignore details))
                        (when (and (eq point :after-validate)
                                   (not injected))
                          (setf injected t)
                          (sb-posix:mkdir (disk "dest") #o755)
                          (sb-posix:symlink "/outside" (disk "dest/sub"))))))
                (with-error (code)
                  (run "archive.extract" '("parent-swap.tar") :to "dest")
                  (expect code :to-equal "refusal.outside-workspace"))
                (expect injected :to-be t)
                (expect (kind "dest/link") :to-be :absent)
                (expect (kind "dest/payload") :to-be :absent))
           (when (member (kind "dest/link") '(:file :symlink))
             (sb-posix:unlink (disk "dest/link")))
           (when (member (kind "dest/payload") '(:file :symlink))
             (sb-posix:unlink (disk "dest/payload")))
           (when (eq (kind "dest/sub") :symlink)
             (sb-posix:unlink (disk "dest/sub")))
           (when (eq (kind "dest") :directory)
             (sb-posix:rmdir (disk "dest"))))
         (expect (kind "dest/sub") :to-be :absent)
         (expect (kind "dest") :to-be :absent))))))
