(in-package #:aitools.edit.test)

(describe "aitools file commands: remaining refusals and areas"
          (it "refuses to merge a directory into a file and to overwrite a directory"
              (with-edit-workspace ()
                                   (put "a.txt" (format nil "a~%"))
                                   (sb-posix:mkdir (disk "d") #o755)
                                   (with-error (code message) (run "move" '("d" "a.txt") :overwrite t :expect-hash (list (format nil "a.txt=~A" (hash "a.txt"))))
                                               (expect code :to-equal "refusal.exists")
                                               (expect message :to-equal "a.txt already exists (directories are never merged)"))
                                   (with-error (code message) (run "move" '("a.txt" "d") :overwrite t)
                                               (expect code :to-equal "refusal.exists")
                                               (expect message :to-equal "d already exists"))))

          (it "refuses a .gz of a symlink"
              (with-edit-workspace ()
                                   (put "a.txt" "a")
                                   (sb-posix:symlink "a.txt" (disk "lnk"))
                                   (with-error (code) (run "archive.create" '("s.gz" "lnk"))
                                               (expect code :to-equal "argument.invalid"))))

          (it "extracts into the workspace root itself"
              (with-edit-workspace ()
                                   (put "a.tar" (aitools.text.domain:write-tar (list (member-file "x.txt" "X"))))
                                   (multiple-value-bind (kind fields) (run "archive.extract" '("a.tar") :to ".")
                                     (expect kind :to-be :ok)
                                     (expect (changes fields) :to-equal '(("x.txt" "created"))))
                                   (expect (text "x.txt") :to-equal "X")))

          (it "refuses a --root that is a file for mktemp"
              (with-edit-workspace ()
                                   (put "a.txt" "a")
                                   (let ((result nil))
                                     (run-edit-command *ports* "mktemp" '() '() :root (disk "a.txt")
                                                       :on-ok (lambda (fields) (setf result fields))
                                                       :on-error (lambda (code message &rest keys) (declare (ignore keys)) (setf result (list code message)))
                                                       :on-partial #'identity)
                                     (expect (first result) :to-equal "argument.invalid")
                                     (expect (search "is not a usable directory" (second result)) :to-be-truthy))))

          (it "keeps going when an expired mktemp entry cannot be removed"
              (with-edit-workspace ()
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
              (with-edit-workspace ()
                                   (let ((path (disk "huge.bin")))
                                     (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-does-not-exist :create))
                                     (sb-posix:truncate path (1+ aitools.edit.application:+max-input-bytes+))
                                     (with-error (code message) (run "write" '("out.bin") :content-file (list "huge.bin"))
                                                 (expect code :to-equal "refusal.too-large")
                                                 (expect (search "--content-file huge.bin exceeds" message) :to-be-truthy))
                                     (expect (kind "out.bin") :to-be :absent))))
          (it "refuses malformed table.set JSON without changing a CSV"
              (with-edit-workspace ()
                (put "t.csv" (format nil "id,name~%1,x~%"))
                (setf *stdin* (bytes "{nope"))
                (let ((before (snapshot)))
                  (with-error (code) (run "table.set" (list "t.csv") :stdin t)
                    (expect code :to-equal "input.syntax-error"))
                  (expect-unchanged before)
                  (expect (text "t.csv") :to-equal (format nil "id,name~%1,x~%")))))
          (it "reports the protected state-directory refusal without changing files"
              (with-edit-workspace ()
                (let ((*home* (disk "protected/custom-state/aitools")))
                  (put "protected/ordinary" "keep")
                  (expect (run "write" '("initialized") :content '("ready")) :to-be :ok)
                  (let* ((before (snapshot))
                         (count (count-if (lambda (entry)
                                            (or (string= (first entry) "protected")
                                                (and (>= (length (first entry)) 10)
                                                     (string= "protected/" (first entry) :end2 10))))
                                          before)))
                    (with-error (code message)
                        (run "rm" '("protected") :recursive t
                             :expect-count (write-to-string count))
                      (expect code :to-equal "refusal.outside-workspace")
                      (expect (search "is inside the aitools state directory" message) :to-be-truthy))
                    (expect-unchanged before)))))
          (it "refuses to remove a temporary file outside the workspace"
              (with-edit-workspace ()
                (let ((temporary (mktemp-path)))
                  (unwind-protect
                      (let ((before (snapshot)))
                        (expect (probe-file temporary) :to-be-truthy)
                        (with-error (code message)
                            (run "rm" (list temporary))
                          (expect code :to-equal "refusal.outside-workspace")
                          (expect message :to-equal "rm requires a workspace target"))
                        (expect-unchanged before)
                        (expect (probe-file temporary) :to-be-truthy))
                    (when (probe-file temporary)
                      (delete-file temporary)))))))
