(in-package #:aitools.e2e.test)

(defun md-ok (workspace arguments command &rest options)
  (let ((envelope (apply #'run-ok workspace arguments options)))
    (expect envelope :to-match-envelope "command" command "status" "ok")
    envelope))

(defun md-error (workspace arguments command code &rest options)
  (let* ((result (apply #'aitools workspace arguments options))
         (envelope (aitools-envelope result))
         (error (jget envelope "error"))
         (expected-exit (aitools.protocol.domain:error-code-exit-code code)))
    (expect (aitools-exit-code result) :to-be expected-exit)
    (expect (aitools-stream result) :to-be :stderr)
    (expect envelope :to-match-envelope "command" command "status" "error")
    (expect (jget error "code") :to-equal code)
    (expect (plusp (length (jlist (jget error "repairs")))) :to-be t)
    envelope))

(defun md-fixture-text ()
  (uiop:read-file-string (asdf:system-relative-pathname "aitools/test/md" "../../../t/e2e/md-fixture.md")
                         :external-format
                         :utf-8))

(defun md-nth-position (source needle occurrence)
  (let ((cursor 0)
        (position nil))
    (dotimes (index occurrence)
      (setf position (search needle source :start2 cursor))
      (unless position
        (error "fixture lacks occurrence ~D of ~S" (1+ index) needle))
      (setf cursor (1+ position)))
    position))

(defun md-crlf (text)
  (with-output-to-string (stream)
    (loop for character across text
          do (progn
               (when (char= character #\Newline)
                 (write-char #\Return stream))
               (write-char character stream)))))

(defun md-splice-text (source start end replacement)
  (concatenate 'string (subseq source 0 start) replacement (subseq source end)))

(describe "aitools e2e md commands"
          (define-org-case ("md headings enumerate parsed headings and read complete sections" ws)
                           (let* ((source (md-fixture-text))
                                  (before (put ws "sections.md" source))
                                  (headings (md-ok ws '("md" "headings" "sections.md") "md headings"))
                                  (entries (jlist (jget headings "headings"))))
                             (expect (plusp (length (file-octets ws "sections.md"))) :to-be t)
                             (expect
                              (mapcar
                               (lambda (entry)
                                 (list (jget entry "section")
                                       (jget entry "heading")
                                       (jget entry "level")
                                       (jget entry "line")))
                               entries)
                              :to-equal
                              '((1 "親" 1 4) (2 "重複" 2 8)
                                (3 "深い節" 3 12)
                                (4 "重複" 2 16)
                                (5 "概要" 1 24)
                                (6 "終端" 1 29)))
                             (let* ((start (md-nth-position source "# 親" 1))
                                    (end (md-nth-position source "概要" 1))
                                    (parent (md-ok ws '("md" "read" "sections.md" "1") "md read")))
                               (expect (jget parent "section") :to-be 1)
                               (expect (jget parent "heading") :to-equal "親")
                               (expect (jget parent "content") :to-equal (subseq source start end)))
                             (let* ((start (md-nth-position source "## 重複" 2))
                                    (end (md-nth-position source "概要" 1))
                                    (duplicate (md-ok ws '("md" "read" "sections.md" "4") "md read")))
                               (expect
                                (list (jget duplicate "section")
                                      (jget duplicate "heading")
                                      (jget duplicate "level")
                                      (jget duplicate "line"))
                                :to-equal
                                '(4 "重複" 2 16))
                               (expect (jget duplicate "content") :to-equal (subseq source start end)))
                             (md-error ws '("md" "read" "sections.md" "99") "md read" "selection.no-match")
                             (md-error ws '("md" "read" "sections.md" "zero") "md read" "argument.invalid")
                             (expect (file-octets ws "sections.md") :to-equalp (octets source))))
          (define-org-case ("md replace journals the exact section splice and rejects a stale hash" ws)
                           (let* ((source (md-fixture-text))
                                  (original (octets source))
                                  (replacement (format nil "## 重複~%~%更新された節 日本語。~%~%")))
                             (put ws "sections.md" source)
                             (let* ((headings (md-ok ws '("md" "headings" "sections.md") "md headings"))
                                    (hash (jget headings "hash"))
                                    (start (md-nth-position source "## 重複" 2))
                                    (end (md-nth-position source "概要" 1))
                                    (expected (octets (md-splice-text source start end replacement)))
                                    (changed
                                     (md-ok ws
                                            (list "md" "replace" "sections.md" "4" "--expect-hash" hash "--stdin")
                                            "md replace"
                                            :stdin
                                            replacement))
                                    (op-id (jget changed "op_id")))
                               (expect
                                (list (jget changed "section")
                                      (jget changed "heading")
                                      (jget changed "level")
                                      (jget changed "line"))
                                :to-equal
                                '(4 "重複" 2 16))
                               (expect (stringp op-id) :to-be t)
                               (expect (file-octets ws "sections.md") :to-equalp expected)
                               (run-ok ws (list "undo" op-id))
                               (expect (file-octets ws "sections.md") :to-equalp original)
                               (put ws "sections.md" (concatenate 'string source "stale~%"))
                               (let ((stale (file-octets ws "sections.md")))
                                 (md-error ws
                                           (list "md" "replace" "sections.md" "4" "--expect-hash" hash "--stdin")
                                           "md replace"
                                           "refusal.target-changed"
                                           :stdin
                                           replacement)
                                 (expect (file-octets ws "sections.md") :to-equalp stale)))))
          (define-org-case ("md replace stages CRLF multibyte sections in a transaction" ws)
                           (let* ((source (md-crlf (md-fixture-text)))
                                  (original (octets source))
                                  (replacement
                                   (format nil
                                           "## 重複~C~C~C~C差し替え 漢字。~C~C~C~C"
                                           #\Return
                                           #\Newline
                                           #\Return
                                           #\Newline
                                           #\Return
                                           #\Newline
                                           #\Return
                                           #\Newline)))
                             (put ws "sections-crlf.md" source)
                             (let* ((headings (md-ok ws '("md" "headings" "sections-crlf.md") "md headings"))
                                    (hash (jget headings "hash"))
                                    (start (md-nth-position source "## 重複" 2))
                                    (end (md-nth-position source "概要" 1))
                                    (expected (octets (md-splice-text source start end replacement)))
                                    (tx (jget (run-ok ws '("tx" "begin")) "tx"))
                                    (arguments
                                     (list "md"
                                           "replace"
                                           "sections-crlf.md"
                                           "4"
                                           "--expect-hash"
                                           hash
                                           "--stdin"
                                           "--tx"
                                           tx))
                                    (staged (md-ok ws arguments "md replace" :stdin replacement)))
                               (expect (jget staged "tx") :to-equal tx)
                               (expect (jget staged "tx_op") :to-be 1)
                               (expect (file-octets ws "sections-crlf.md") :to-equalp original)
                               (let* ((committed (run-ok ws (list "tx" "commit" tx)))
                                      (op-id (jget committed "op_id")))
                                 (expect (stringp op-id) :to-be t)
                                 (expect (file-octets ws "sections-crlf.md") :to-equalp expected)
                                 (run-ok ws (list "undo" op-id))
                                 (expect (file-octets ws "sections-crlf.md") :to-equalp original)))))
          (it "md replace refuses paths outside the workspace and inside git metadata"
              (let ((name "md replace refuses paths outside the workspace and inside git metadata"))
                (with-progress (name)
                               (let ((*current-case* (list 0 name)))
                                 (aitools-binary)
                                 (call-with-workspace
                                  (lambda (ws)
                                    (let* ((source (md-fixture-text))
                                           (replacement (format nil "# Changed~%~%replacement~%")))
                                      (put ws "../outside.md" source)
                                      (let* ((info (run-ok ws '("info" "../outside.md")))
                                             (hash (jget info "hash"))
                                             (before (file-octets ws "../outside.md")))
                                        (md-error ws
                                                  (list "md"
                                                        "replace"
                                                        "../outside.md"
                                                        "1"
                                                        "--expect-hash"
                                                        hash
                                                        "--stdin")
                                                  "md replace"
                                                  "refusal.outside-workspace"
                                                  :stdin
                                                  replacement)
                                        (expect (file-octets ws "../outside.md") :to-equalp before))
                                      (let* ((before (file-octets ws ".git/config"))
                                             (info (run-ok ws '("info" ".git/config")))
                                             (hash (jget info "hash")))
                                        (md-error ws
                                                  (list "md" "replace" ".git/config" "1" "--expect-hash" hash "--stdin")
                                                  "md replace"
                                                  "refusal.outside-workspace"
                                                  :stdin
                                                  replacement)
                                        (expect (file-octets ws ".git/config") :to-equalp before))))
                                  :git
                                  t)))))
          (define-org-case ("md headings/read/replace reject documents above the byte limit" ws)
                           (let* ((replacement (format nil "# Revised~%~%replacement~%"))
                                  (large-content (make-string (1+ (* 16 1024 1024)) :initial-element #\Space))
                                  (hash (make-string 64 :initial-element #\0)))
                             (put ws "large.md" large-content)
                             (dolist (entry '((("md" "headings" "large.md") "md headings")
                                              (("md" "read" "large.md" "1") "md read")))
                               (let ((envelope
                                       (md-error ws (first entry) (second entry) "refusal.too-large")))
                                 (expect (jget envelope "next_commands") :to-be nil)))
                             (md-error ws
                                       (list "md" "replace" "large.md" "1" "--expect-hash" hash "--stdin")
                                       "md replace"
                                       "refusal.too-large"
                                       :stdin
                                       replacement)
                             (expect (file-octets ws "large.md") :to-equalp (octets large-content))))
)
