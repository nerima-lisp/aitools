(in-package #:aitools.edit.test)

(describe "aitools write, split and transcode"
  (it "refuses to replace a file without --overwrite and concatenates with --separator"
    (call-with-edit-workspace (lambda () (put "a.txt" "old")
      (with-error (code) (run "write" '("a.txt") :content '("new")) (expect code :to-equal "refusal.exists"))
      (put "p1" "one")
      (put "p2" "two")
      (expect (run "write" '("joined.txt") :content-file (list (disk "p1") (disk "p2")) :separator "--") :to-be :ok)
      (expect (text "joined.txt") :to-equal "one--two")
      (expect (run "write" '("a.txt") :content '("new") :overwrite t :expect-hash (list (hash "a.txt"))) :to-be :ok)
      (expect (text "a.txt") :to-equal "new"))))
  (it "keeps an ordinary hash-guarded edit uncapped by default"
    (call-with-edit-workspace
      (lambda ()
        (let ((original
                (format nil "~A marker~%"
                        (make-string (* 1024 1024) :initial-element (code-char 97)))))
          (put "a.txt" original)
          (expect
            (run "edit" (list "a.txt")
                 :old "marker" :new "changed"
                 :expect-hash (list (hash "a.txt")))
            :to-be :ok)
          (expect (search "changed" (text "a.txt")) :to-be-truthy)
          (expect (search "marker" (text "a.txt")) :to-be nil)))))

  (it "splits by lines and refuses missing inputs or existing pieces"
    (call-with-edit-workspace (lambda () (put "a.txt" (format nil "1~%2~%3~%"))
      (let ((before (snapshot)))
        (with-error (code)
            (run "split" '("missing.txt") :lines "1")
          (expect code :to-equal "input.not-found"))
        (expect-unchanged before))
      (multiple-value-bind (kind fields) (run "split" '("a.txt") :lines "2")
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (change) (list (json-field change "path")
                                               (json-field change "start_line")
                                               (json-field change "lines")))
                        (field fields "changes"))
                :to-equal '(("a.txt.001" 1 2) ("a.txt.002" 3 1))))
      (expect (text "a.txt.002") :to-equal (format nil "3~%"))
      (let ((before (snapshot)))
        (with-error (code)
            (run "split" '("a.txt") :lines "1")
          (expect code :to-equal "refusal.exists"))
        (expect-unchanged before)))))

  (it-each (("shift_jis") ("euc-jp") ("utf-16le") ("utf-16be"))
      "round-trips ~A through UTF-8 byte for byte"
      (encoding)
    (call-with-edit-workspace (lambda () (let ((original (aitools.text.domain:encode-string/k (format nil "日本語テキスト~%abc~%")
                                                            (aitools.text.domain:find-encoding encoding)
                                                            :on-encoded (lambda (octets replaced) (declare (ignore replaced)) octets))))
        (put "j.txt" original)
        (multiple-value-bind (kind fields) (run "transcode" '("j.txt") :from encoding)
          (expect kind :to-be :ok)
          (expect (field fields "to") :to-equal "utf-8"))
        (expect (text "j.txt") :to-equal (format nil "日本語テキスト~%abc~%"))
        (expect (run "transcode" '("j.txt") :from "utf-8" :to encoding) :to-be :ok)
        (expect (octets-of "j.txt") :to-equalp original)))))
  (flet ((generator-for (encoding)
           (cl-weave:gen-string
            :min-length 0
            :max-length 80
            :alphabet
            (case encoding
              (:utf-8 (format nil "abcéλ日本語テキスト😀~%"))
              (:shift_jis (format nil "abc日本語テキスト~%"))
              (:euc-jp (format nil "abc日本語テキスト~%"))
              (:iso-8859-1 (format nil "abcéÿ~%"))
              ((:utf-16le :utf-16be)
               (format nil "abcéλ日本語テキスト😀~%")))))
         (assert-transcode-round-trip (content encoding)
           (let* ((name (aitools.text.domain:encoding-name encoding))
                  (original
                    (aitools.text.domain:encode-string/k
                     content
                     encoding
                     :on-encoded
                     (lambda (octets replaced)
                       (expect replaced :to-be 0)
                       octets))))
             (call-with-edit-workspace
              (lambda ()
                (put "j.txt" original)
                (multiple-value-bind (kind fields)
                    (run "transcode" '("j.txt") :from name)
                  (expect kind :to-be :ok)
                  (expect (field fields "to") :to-equal "utf-8"))
                (expect (text "j.txt") :to-equal content)
                (multiple-value-bind (kind fields)
                    (run "transcode" '("j.txt") :from "utf-8" :to name)
                  (expect kind :to-be :ok)
                  (expect (field fields "to") :to-equal name))
                (expect (octets-of "j.txt") :to-equalp original))))))
    (it-property
     "round-trips generated UTF-8 text"
     ((content (generator-for :utf-8)))
     (assert-transcode-round-trip content :utf-8))
    (it-property
     "round-trips generated Shift_JIS text"
     ((content (generator-for :shift_jis)))
     (assert-transcode-round-trip content :shift_jis))
    (it-property
     "round-trips generated EUC-JP text"
     ((content (generator-for :euc-jp)))
     (assert-transcode-round-trip content :euc-jp))
    (it-property
     "round-trips generated ISO-8859-1 text"
     ((content (generator-for :iso-8859-1)))
     (assert-transcode-round-trip content :iso-8859-1))
    (it-property
     "round-trips generated UTF-16LE text"
     ((content (generator-for :utf-16le)))
     (assert-transcode-round-trip content :utf-16le))
    (it-property
     "round-trips generated UTF-16BE text"
     ((content (generator-for :utf-16be)))
     (assert-transcode-round-trip content :utf-16be)))


  (it "refuses unrepresentable characters unless --replace-unmappable"
    (call-with-edit-workspace (lambda () (put "e.txt" (format nil "caf~C 😀~%" (code-char #xE9)))
      (let ((before (snapshot)))
        (with-error (code) (run "transcode" '("e.txt") :to "iso-8859-1") (expect code :to-equal "argument.invalid"))
        (expect-unchanged before))
      (multiple-value-bind (kind fields) (run "transcode" '("e.txt") :to "iso-8859-1" :replace-unmappable t)
        (expect kind :to-be :ok)
        (expect (field fields "replaced") :to-be 1))
      (expect (octets-of "e.txt") :to-equalp (octet-vector 99 97 102 #xE9 32 63 10)))))
  (it "reports a missing source when transcoding"
    (call-with-edit-workspace (lambda () (with-error (code)
          (run "transcode" '("missing.txt") :to "utf-8")
        (expect code :to-equal "input.not-found"))
      (expect (kind "missing.txt") :to-be :absent)))))

(describe "aitools text writes and undo"
  (it "restores the original bytes for each text command's op"
    (call-with-edit-workspace (lambda () (put "a.txt" (format nil "b~%a~%b~%"))
      (put "c.json" (format nil "{~%  \"k\": 1~%}~%"))
      (let ((before (snapshot)))
        (dolist (call (list (list "edit" '("a.txt") :old "a" :new "z")
                            (list "insert" '("a.txt") :at "start" :content "top")
                            (list "replace" '("b" "B" "a.txt") :expect-count "2")
                            (list "transform" '("a.txt") :op '("sort"))
                            (list "move-lines" '("a.txt") :match "^a$" :expect-count "1" :to-position "start")
                            (list "transcode" '("a.txt") :to "utf-16le")
                            (list "doc.set" '("c.json" "/k" "2"))
                            (list "split" '("a.txt") :lines "1")
                            (list "write" '("new.txt") :content '("n"))))
          (multiple-value-bind (kind fields) (apply #'run call)
            (expect kind :to-be :ok)
            (expect (undo (field fields "op_id")) :to-be :committed)
            (expect (snapshot) :to-equal before))))))))
