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
