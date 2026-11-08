(in-package #:aitools.store.test)

(defun bounded-child-result (thunk)
  (let ((pid (call-in-child-process thunk)) (reaped nil))
    (unwind-protect
         (loop repeat 100
               do (multiple-value-bind (finished status) (sb-posix:waitpid pid sb-posix:wnohang)
                    (when (plusp finished)
                      (setf reaped t)
                      (return (and (sb-posix:wifexited status) (sb-posix:wexitstatus status)))))
                  (sleep 0.01)
               finally (return :timeout))
      (unless reaped (kill-child pid)))))

(describe "aitools.store concurrent deletion guards"
  (it "replays legacy deletion intents without optional identity fields"
    (call-with-temp-store (lambda (store) (seed-state store)
      (put-file store "legacy" "before")
      (forge-intent store (list (make-intent-step :op :unlink :path "legacy")))
      (expect (recover store) :to-be :recovered)
      (expect (disk-text store "legacy") :to-be :absent)
      (expect (intent-files store) :to-equal '())) :io nil))

  (it "bounds exact, empty, oversized and growing reads"
    (call-with-temp-store (lambda (store) (put-file store "victim" "abc")
      (put-file store "empty" "")
      (expect (length (view-read-file (disk-view store) "victim" :max-bytes 3)) :to-be 3)
      (expect (length (view-read-file (disk-view store) "empty" :max-bytes 0)) :to-be 0)
      (expect (handler-case (view-read-file (disk-view store) "victim" :max-bytes 2)
                (store-io-error () :refused)) :to-be :refused)
      (let ((aitools.store.application::*fault-hook*
              (lambda (point &rest details)
                (when (and (eq point :after-read-stat)
                           (string= (first details) (disk-path store "victim")))
                  (with-open-file (out (disk-path store "victim") :direction :output
                                       :if-exists :append)
                    (write-string "growth" out))))))
        (expect (handler-case (view-read-file (disk-view store) "victim" :max-bytes 3)
                  (store-io-error () :refused)) :to-be :refused))
      (expect (disk-text store "victim") :to-equal "abcgrowth")) :io nil))

  (it "retains a committed intent until its original deletion target is restored"
    (call-with-temp-store (lambda (store) (put-file store "victim" "before")
      (call-with-fault-at :after-intent (lambda () (commit store (list (delete-request "victim" :max-bytes 6)))))
      (sb-posix:rename (disk-path store "victim") (disk-path store "original"))
      (put-file store "victim" "before")
      (expect (handler-case (recover store) (store-committed-error () :refused)) :to-be :refused)
      (expect (disk-text store "victim") :to-equal "before")
      (expect (length (intent-files store)) :to-be 1)
      (sb-posix:rename (disk-path store "original") (disk-path store "victim"))
      (expect (recover store) :to-be :recovered)
      (expect (disk-text store "victim") :to-be :absent)
      (expect (intent-files store) :to-equal '())) :io nil))

  (it "refuses a file swapped to a FIFO between lstat and open without blocking"
    (call-with-temp-store
     (lambda (store)
       (put-file store "victim" "before")
       (let* ((path (disk-path store "victim"))
              (base-boundaries (store-boundaries store))
              (read-file (store-operation base-boundaries :read-file))
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 base-boundaries
                 :read-file
                 (lambda (name &rest keys)
                   (when (string= name path)
                     (sb-posix:unlink path)
                     (sb-posix:mkfifo path #o600))
                   (apply read-file name keys))))
              (guarded (make-store boundaries (store-root store) (state-home-of store))))
         (expect
          (bounded-child-result
           (lambda ()
             (handler-case
                 (progn (view-read-file (disk-view guarded) "victim")
                        (error "FIFO read was accepted"))
               (store-io-error () nil))))
          :to-be 0)))))


  (it "preserves a same-content replacement inode after validation"
    (call-with-temp-store (lambda (store) (put-file store "victim" "before")
      (let ((aitools.store.application::*fault-hook*
              (lambda (point &rest details)
                (declare (ignore details))
                (when (eq point :after-validate)
                  (sb-posix:rename (disk-path store "victim") (disk-path store "original"))
                  (put-file store "victim" "before")))))
        (expect (commit store (list (delete-request "victim"))) :to-be :rejected))
      (expect (disk-text store "victim") :to-equal "before")
      (expect (intent-files store) :to-equal '())) :io nil)))

(defun io-failure (thunk)
  "(operation detail) of the STORE-IO-ERROR THUNK signals, or :OK."
  (handler-case (progn (funcall thunk) :ok)
    (store-io-error (condition) (list (store-io-error-operation condition) (store-io-error-detail condition)))))

(describe "aitools.store posix store-io primitives"
  (it "classifies a FIFO as :other and reports an unreadable parent instead of calling it absent"
    (call-with-temp-store (lambda (store) (let ((io (store-boundaries store))
            (locked (disk-path store "locked")))
        (sb-posix:mkfifo (disk-path store "pipe") #o600)
        (expect (funcall (store-operation io :lstat) (disk-path store "pipe")) :to-be :other)
        (sb-posix:mkdir locked #o700)
        (sb-posix:chmod locked #o000)
        (unwind-protect
             (expect (io-failure (lambda () (funcall (store-operation io :lstat) (concatenate 'string locked "/x"))))
                     :to-equal '("lstat" "Permission denied"))
          (sb-posix:chmod locked #o700)))) :io nil))

  (it-each ((65535) (65536) (65537) (131072))
      "reads a ~D-octet file whole"
      (size)
    (call-with-temp-store (lambda (store) (let ((path (disk-path store "data.bin")))
        (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (make-array size :element-type '(unsigned-byte 8) :initial-element 9) out))
        (let ((octets (funcall (store-operation (store-boundaries store) :read-file) path)))
          (expect (length octets) :to-be size)
          (expect (count 9 octets) :to-be size)))) :io nil))

  (it "changes the mode and time of a file its owner cannot open, by name"
    (call-with-temp-store (lambda (store) (let ((io (store-boundaries store))
            (path (disk-path store "sealed.txt")))
        (put-file store "sealed.txt" "s" :mode #o000)
        (funcall (store-operation io :set-mtime) path 1577934245)
        (funcall (store-operation io :chmod) path #o640)
        (expect (disk-mode store "sealed.txt") :to-be #o640)
        (expect (sb-posix:stat-mtime (sb-posix:lstat path)) :to-be 1577934245))) :io nil))

  (it "refuses a time change on a directory and a mode change of a missing path"
    (call-with-temp-store (lambda (store) (let ((io (store-boundaries store)))
        (sb-posix:mkdir (disk-path store "d") #o755)
        (expect (io-failure (lambda () (funcall (store-operation io :set-mtime) (disk-path store "d") 1)))
                :to-equal '("utimes" "not a regular file"))
        (expect (io-failure (lambda () (funcall (store-operation io :chmod) (disk-path store "none") #o600)))
                :to-equal '("open" "No such file or directory")))) :io nil))
  (it "rejects symlink mode changes in the shared-inode guard"
    (expect (io-failure
             (lambda ()
               (aitools.store.infrastructure::%refuse-shared-inode "chmod" "link" sb-posix:s-iflnk 1)))
            :to-equal '("chmod" "is a symlink")))

  (it "walks pinned parents below a root given with a trailing slash"
    (call-with-temp-store (lambda (store) (let ((io (make-posix-store-io :root (concatenate 'string (store-root store) "/"))))
        (funcall (store-operation io :mkdir) (disk-path store "a"))
        (funcall (store-operation io :mkdir) (disk-path store "a/b") :mode #o700)
        (funcall (store-operation io :create-file) (disk-path store "a/b/c.txt") (bytes "deep"))
        (expect (disk-text store "a/b/c.txt") :to-equal "deep")
        (expect (disk-mode store "a/b/c.txt") :to-be #o600)
        (expect (disk-mode store "a/b") :to-be #o700)
        (sb-posix:symlink "a" (disk-path store "via"))
        ;; Darwin checks O_DIRECTORY first (ENOTDIR), Linux O_NOFOLLOW (ELOOP).
        (expect
         (member
          (io-failure
           (lambda ()
             (funcall (store-operation io :create-file)
                      (disk-path store "via/b/x")
                      (bytes "x"))))
          '(("open" "Not a directory") ("open" "Too many levels of symbolic links"))
          :test #'equal)
         :to-be-truthy)
        (expect (disk-text store "a/b/x") :to-be :absent))) :io nil)))
(describe "aitools.store committed error pass-through"
  (it "preserves the op id of an existing committed error"
    (call-with-temp-store
     (lambda (plain)
       (let* ((base (store-boundaries plain))
              (outer-op-id (format-op-id (get-universal-time) "00000001"))
              (inner-op-id (format-op-id (get-universal-time) "00000002"))
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 base :mkdir
                 (lambda (path &rest keys)
                   (declare (ignore keys))
                   (error 'store-committed-error :op-id inner-op-id :operation "mkdir" :path path))))
              (store (make-store boundaries (store-root plain) (state-home-of plain)))
              (intent
                (make-intent
                 :op-id outer-op-id
                 :steps (list (make-intent-step :op :mkdir :path "newdir" :mode #o700))
                 :journal-entry
                 (make-journal-entry :op-id outer-op-id :argv '("test") :time "test" :changes '()))))
         (expect
          (handler-case
              (progn (aitools.store.application::%complete-intent store intent) :returned)
            (store-committed-error (condition)
              (store-committed-error-op-id condition)))
          :to-equal inner-op-id))) :io nil)))
