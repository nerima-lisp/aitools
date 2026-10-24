(in-package #:aitools.edit.test)

(describe "aitools file commands: remaining refusals and areas"
  (it "refuses to merge a directory into a file and to overwrite a directory"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (sb-posix:mkdir (disk "d") #o755)
      (with-error (code message) (run "move" '("d" "a.txt") :overwrite t :expect-hash (list (format nil "a.txt=~A" (hash "a.txt"))))
        (expect code :to-equal "refusal.exists")
        (expect message :to-equal "a.txt already exists (directories are never merged)"))
      (with-error (code message) (run "move" '("a.txt" "d") :overwrite t)
        (expect code :to-equal "refusal.exists")
        (expect message :to-equal "d already exists"))))

  (it "refuses a .gz of a symlink"
    (with-workspace ()
      (put "a.txt" "a")
      (sb-posix:symlink "a.txt" (disk "lnk"))
      (with-error (code) (run "archive.create" '("s.gz" "lnk"))
        (expect code :to-equal "argument.invalid"))))

  (it "extracts into the workspace root itself"
    (with-workspace ()
      (put "a.tar" (aitools.text.domain:write-tar (list (member-file "x.txt" "X"))))
      (multiple-value-bind (kind fields) (run "archive.extract" '("a.tar") :to ".")
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("x.txt" "created"))))
      (expect (text "x.txt") :to-equal "X")))

  (it "refuses a --root that is a file for mktemp"
    (with-workspace ()
      (put "a.txt" "a")
      (let ((result nil))
        (run-edit-command *ports* "mktemp" '() '() :root (disk "a.txt")
                          :on-ok (lambda (fields) (setf result fields))
                          :on-error (lambda (code message &rest keys) (declare (ignore keys)) (setf result (list code message))))
        (expect (first result) :to-equal "argument.invalid")
        (expect (search "is not a usable directory" (second result)) :to-be-truthy))))

  (it "keeps going when an expired mktemp entry cannot be removed"
    (with-workspace ()
      (let* ((temporary (mktemp-path :dir t))
             (inner (concatenate 'string temporary "/kept")))
        (with-open-file (out (sb-ext:parse-native-namestring inner) :direction :output :if-does-not-exist :create))
        (sb-posix:utimes temporary 0 0)
        (sb-posix:chmod temporary #o555)
        (unwind-protect
             (progn
               (expect (search "/tmp/tmp." (mktemp-path)) :to-be-truthy)
               (if (zerop (sb-posix:getuid))
                   (expect (probe-file inner) :to-be nil) ; root removes it regardless of the mode
                   (expect (probe-file inner) :to-be-truthy)))
          (when (probe-file (concatenate 'string temporary "/"))
            (sb-posix:chmod temporary #o755))))))

  (it "refuses a --content-file larger than the input limit without reading it"
    (with-workspace ()
      (let ((path (disk "huge.bin")))
        (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-does-not-exist :create))
        (sb-posix:truncate path (1+ aitools.edit.application:+max-input-bytes+))
        (with-error (code message) (run "write" '("out.bin") :content-file (list "huge.bin"))
          (expect code :to-equal "refusal.too-large")
          (expect (search "--content-file huge.bin exceeds" message) :to-be-truthy))
        (expect (kind "out.bin") :to-be :absent)))))

(defun call-with-store-io (overrides function)
  "Call FUNCTION with *PORTS* opening stores whose I/O port is the production
one with OVERRIDES (COPY-STORE-IO keywords) in place."
  (let ((*ports* (make-edit-ports
                  :workspace-host (aitools.edit.application::edit-ports-workspace-host *ports*)
                  :open-store (lambda (root)
                                (aitools.store.application:make-store
                                 (apply #'aitools.store.application::copy-store-io
                                        (aitools.store.infrastructure:make-posix-store-io) overrides)
                                 root *home* :temporary (%temporary-area-p root)))
                  :text-source (aitools.edit.application::edit-ports-text-source *ports*)
                  :read-stdin-octets #'%stdin-port
                  :unix-now #'aitools.edit.infrastructure:unix-now)))
    (funcall function)))

(describe "aitools mktemp against a racing mktemp area"
  (it "draws another name when the first is taken"
    (with-workspace ()
      (let* ((first (mktemp-path))
             (tmp (subseq first 0 (position #\/ first :from-end t)))
             (taken (concatenate 'string tmp "/tmp.aaaaaaaaaaaa"))
             (draws (list "aaaaaaaaaaaa" "bbbbbbbbbbbb")))
        (with-open-file (out (sb-ext:parse-native-namestring taken) :direction :output :if-does-not-exist :create)
          (write-string "kept" out))
        (call-with-store-io (list :random-hex (lambda (count) (declare (ignore count)) (pop draws)))
                            (lambda ()
                              (expect (mktemp-path) :to-equal (concatenate 'string tmp "/tmp.bbbbbbbbbbbb"))))
        (expect (with-open-file (in (sb-ext:parse-native-namestring taken)) (read-line in)) :to-equal "kept"))))

  (it "accepts the area another process created between its check and its mkdir"
    (with-workspace ()
      (let ((real-mkdir (aitools.store.application:store-io-mkdir (aitools.store.infrastructure:make-posix-store-io))))
        (call-with-store-io
         (list :mkdir (lambda (path &rest keys)
                        (apply real-mkdir path keys)
                        (if (search "/tmp" path :from-end t :start2 (max 0 (- (length path) 4)))
                            (error 'aitools.store.application:store-io-error :operation "mkdir" :path path
                                                                             :detail "File exists")
                            nil)))
         (lambda ()
           (let ((path (mktemp-path)))
             (expect (probe-file path) :to-be-truthy))))))))

(describe "aitools copy --recursive of the workspace root"
  (it "places the root's entries below the destination directory"
    (with-workspace ()
      (put "a.txt" "a")
      (put "d/b.txt" "b")
      (multiple-value-bind (kind fields) (run "copy" '("." "x") :recursive t)
        (expect kind :to-be :ok)
        (expect (field fields "files") :to-be 2))
      (expect (text "x/a.txt") :to-equal "a")
      (expect (text "x/d/b.txt") :to-equal "b")
      (expect (kind "xa.txt") :to-be :absent))))

(describe "aitools rm protected roots and recovery"
  (it "refuses the injected home directory without touching its children"
    (with-workspace ()
      (put "fixture-home/keep" "protected")
      (put "ordinary/keep" "ordinary")
      (let* ((host (aitools.workspace.infrastructure:make-host-workspace-host
                    :home-directory (lambda () (disk "fixture-home"))))
             (*ports* (make-edit-ports
                       :workspace-host host
                       :open-store #'open-store
                       :text-source (aitools.edit.application::edit-ports-text-source *ports*)
                       :read-stdin-octets #'%stdin-port
                       :unix-now #'aitools.edit.infrastructure:unix-now))
             (before (snapshot)))
        (expect (run "rm" '("ordinary") :recursive t :dry-run t) :to-be :ok)
        (with-error (code) (run "rm" '("fixture-home") :recursive t :expect-count "2")
          (expect code :to-equal "refusal.outside-workspace"))
        (expect-unchanged before))))

  (it "refuses an ancestor containing a custom state home before deleting ordinary files"
    (with-workspace ()
      (let ((*home* (disk "protected/custom-state/aitools")))
        (put "protected/ordinary" "keep")
        (expect (run "write" '("initialized") :content '("ready")) :to-be :ok)
        (expect (kind "protected/custom-state/aitools") :to-be :directory)
        (let* ((before (snapshot))
               (count (count-if (lambda (entry)
                                  (or (string= (first entry) "protected")
                                      (and (>= (length (first entry)) 10)
                                           (string= "protected/" (first entry) :end2 10))))
                                before)))
          (with-error (code) (run "rm" '("protected") :recursive t
                                 :expect-count (write-to-string count))
            (expect code :to-equal "refusal.outside-workspace"))
          (expect-unchanged before)
          (with-error (code) (run "rm" '("protected") :recursive t :dry-run t)
            (expect code :to-equal "refusal.outside-workspace"))
          (expect-unchanged before)))))

  (it "never traverses an existing outside directory linked from a removed tree"
    (with-workspace ()
      (put "../outside/keep" (octet-vector 0 255 128 10) :mode #o600)
      (put "tree/ordinary" "owned")
      (sb-posix:symlink "../../outside" (disk "tree/link"))
      (let ((before (snapshot))
            (outside (coerce (octets-of "../outside/keep") 'list)))
        (multiple-value-bind (kind fields) (run "rm" '("tree") :recursive t :dry-run t)
          (expect kind :to-be :ok)
          (expect (field fields "entries") :to-be 3)
          (expect (field fields "bytes") :to-be 5))
        (multiple-value-bind (kind fields) (run "rm" '("tree") :recursive t :expect-count "3")
          (expect kind :to-be :ok)
          (expect (kind "tree") :to-be :absent)
          (expect (coerce (octets-of "../outside/keep") 'list) :to-equal outside)
          (expect (mode "../outside/keep") :to-be #o600)
          (expect (undo (field fields "op_id")) :to-be :committed))
        (expect (snapshot) :to-equal before)
        (expect (coerce (octets-of "../outside/keep") 'list) :to-equal outside))))

  (it-each ((:after-intent :throw) (:after-apply-step :throw)
            (:after-apply :throw) (:after-journal :throw) (:after-apply-step :exit))
      "recovers a recursive rm interrupted at ~A with ~A and preserves its undo" (point mode)
    (with-workspace ()
      (put "tree/sub/binary" (octet-vector 0 255 10) :mode #o600)
      (put "keep" "untouched")
      (sb-posix:chmod (disk "tree") #o750)
      (sb-posix:chmod (disk "tree/sub") #o700)
      (sb-posix:symlink "../keep" (disk "tree/link"))
      (let ((before (snapshot))
            (op-id nil)
            (store (open-store *root*)))
        (flet ((interrupt ()
                 (aitools.store.test-support:call-with-fault-at point (lambda () (run "rm" '("tree") :recursive t :expect-count "4")) :mode mode)))
          (if (eq mode :exit)
              (let ((pid (aitools.store.test-support:call-in-child-process #'interrupt)))
                (expect (multiple-value-list (aitools.store.test-support:wait-for-child pid))
                        :to-equal '(:exited 99)))
              (expect (interrupt) :to-be t)))
        (when (eq point :after-intent)
          (expect (snapshot) :to-equal before))
        (when (member point '(:after-apply :after-journal))
          (expect (kind "tree") :to-be :absent))
        (expect (length (aitools.store.application:read-journal store))
                :to-be (if (eq point :after-journal) 1 0))
        (let ((recovered (aitools.store.application:recover/k
                          store
                          :on-rolled-forward (lambda (id) (setf op-id id))
                          :on-discarded (lambda (id) (error "rm intent was discarded: ~A" id))
                          :on-none (lambda () (error "rm intent was not retained"))
                          :on-busy (lambda () (error "rm recovery lock is busy")))))
          (expect (stringp op-id) :to-be t)
          (expect recovered :to-equal (list (cons op-id "rolled-forward"))))
        (expect (kind "tree") :to-be :absent)
        (expect (text "keep") :to-equal "untouched")
        (expect (length (aitools.store.application:read-journal store)) :to-be 1)
        (expect (aitools.store.application:recover/k
                 store
                 :on-rolled-forward (lambda (id) (error "rm recovered twice: ~A" id))
                 :on-discarded (lambda (id) (error "rm intent was discarded: ~A" id))
                 :on-none (lambda () :none)
                 :on-busy (lambda () (error "rm recovery lock is busy")))
                :to-be :none)
        (expect (undo op-id) :to-be :committed)
        (expect (snapshot) :to-equal before)))))
