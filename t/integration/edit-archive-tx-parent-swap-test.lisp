(in-package #:aitools.edit.test)

(describe "archive extraction transaction parent swap"
  (it "rechecks a disk parent swap at archive commit"
    (call-with-edit-workspace
     (lambda ()
       (let* ((test-id
                (car (last (butlast (pathname-directory
                                    (uiop:ensure-directory-pathname *root*))))))
              (target-name (format nil "archive-tx-parent-swap-target-~A" test-id))
              (link-name (format nil "archive-tx-parent-swap-link-~A" test-id))
              (outside-target (disk (format nil "../~A" target-name)))
              (outside-link (disk (format nil "../~A" link-name)))
              (archive
                (aitools.text.domain:write-tar
                 (list (member-file target-name "payload")
                       (aitools.text.domain:make-archive-member
                        :name link-name :kind :symlink :link-target target-name)))))
         (expect (kind (format nil "../~A" target-name)) :to-be :absent)
         (expect (kind (format nil "../~A" link-name)) :to-be :absent)
         (put "dest/marker" "kept")
         (put "parent-swap.tar" archive)
         (let ((tx (begin-tx)))
           (expect (run-in tx "archive.extract" '("parent-swap.tar") :to "dest")
                   :to-be :ok)
           (sb-posix:rename (disk "dest") (disk "dest-before-race"))
           (sb-posix:symlink ".." (disk "dest"))
           (let ((before (snapshot)))
             (unwind-protect
                  (progn
                    (expect (commit-tx tx) :to-equal "refusal.outside-workspace")
                    (expect-unchanged before)
                    (expect (kind (format nil "../~A" target-name)) :to-be :absent)
                    (expect (kind (format nil "../~A" link-name)) :to-be :absent))
               (when (member (kind (format nil "../~A" target-name))
                             '(:file :symlink))
                 (sb-posix:unlink outside-target))
               (when (member (kind (format nil "../~A" link-name))
                             '(:file :symlink))
                 (sb-posix:unlink outside-link))))))))))
