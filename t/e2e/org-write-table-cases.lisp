;;;; t/e2e/org-write-table-cases.lisp
(in-package #:aitools.e2e.test)

(defmacro define-org-table-write-case ((name workspace) &body body)
  `(it ,name
     (with-progress (,name)
       (let ((*current-case* (list 0 ,name)))
         (aitools-binary)
         (call-with-workspace (lambda (,workspace) ,@body))))))

(defparameter +org-table-byte-fixture+
  (concatenate 'string
               "#+TODO: TODO | DONE" (string #\Newline)
               "* TODO 設計" (string #\Tab) "roadmap" (format nil "~C~C" #\Return #\Newline)
               "SCHEDULED: <2026-10-20>" (string #\Newline)
               ":PROPERTIES:" (format nil "~C~C" #\Return #\Newline)
               ":OWNER: 佐藤" (string #\Tab) "qa" (string #\Newline)
               ":TEAM: Org" (format nil "~C~C" #\Return #\Newline)
               ":END:" (string #\Newline)
               "| A | B |" (format nil "~C~C" #\Return #\Newline)
               "|---+---|" (string #\Newline)
               "| 1 | 2 |" (format nil "~C~C" #\Return #\Newline)
               "#+TBLFM: @2$2=2+1"))

(defun org-diff-cut-fixture ()
  (with-output-to-string (stream)
    (write-string "| " stream)
    (write-string (make-string 201 :initial-element #\X) stream)
    (write-string " |" stream)
    (terpri stream)
    (loop repeat 201
          do (write-string "| x |" stream)
             (terpri stream))))

(describe "aitools e2e org table writes"
  (define-org-table-write-case ("org table align and recalculate preserve F-BYTE spans" ws)
    (put ws "byte-table.org" +org-table-byte-fixture+)
    (let ((before (file-octets ws "byte-table.org")))
      (let ((aligned (org-ok ws '("org" "table" "align" "byte-table.org" "--table" "1")
                             "org.table.align")))
        (expect (jget aligned "table") :to-be 1)
        (expect (jget aligned "changed") :to-be nil)
        (expect (file-octets ws "byte-table.org") :to-equalp before))
      (let* ((recalculated (org-ok ws '("org" "table" "recalculate" "byte-table.org" "--table" "1")
                                   "org.table.recalculate"))
             (expected
               (concatenate 'string
                            "#+TODO: TODO | DONE" (string #\Newline)
                            "* TODO 設計" (string #\Tab) "roadmap" (format nil "~C~C" #\Return #\Newline)
                            "SCHEDULED: <2026-10-20>" (string #\Newline)
                            ":PROPERTIES:" (format nil "~C~C" #\Return #\Newline)
                            ":OWNER: 佐藤" (string #\Tab) "qa" (string #\Newline)
                            ":TEAM: Org" (format nil "~C~C" #\Return #\Newline)
                            ":END:" (string #\Newline)
                            "| A | B |" (format nil "~C~C" #\Return #\Newline)
                            "|---+---|" (string #\Newline)
                            "| 1 | 3 |" (format nil "~C~C" #\Return #\Newline)
                            "#+TBLFM: @2$2=2+1")))
        (expect (jget recalculated "table") :to-be 1)
        (expect (jget recalculated "changed") :to-be t)
        (expect-org-write-and-undo ws "byte-table.org" before expected recalculated))))

  (define-org-table-write-case ("org table align normalizes mixed tables without touching surrounding text" ws)
    (put ws "align.org"
         (format nil
                 (concatenate 'string
                              "Before~%# keep this comment~%#+FR-013-UNKNOWN: retained~%"
                              "  | key | value~%|--+-------~%| a | 1~%"
                              "  |-----+-------~%  | bb~% |---+--+--~%"
                              "| ccc | 22 | z~%After~%")))
    (let* ((before (file-octets ws "align.org"))
           (aligned (org-ok ws '("org" "table" "align" "align.org" "--table" "1")
                            "org.table.align"))
           (expected
             (format nil
                     (concatenate 'string
                                  "Before~%# keep this comment~%#+FR-013-UNKNOWN: retained~%"
                                  "  | key | value |   |~%  |-----+-------+---|~%"
                                  "  | a   |     1 |   |~%  |-----+-------+---|~%"
                                  "  | bb  |       |   |~%  |-----+-------+---|~%"
                                  "  | ccc |    22 | z |~%After~%"))))
      (expect (jget aligned "table") :to-be 1)
      (expect (jget aligned "changed") :to-be t)
      (expect-org-write-and-undo ws "align.org" before expected aligned)))

  (define-org-table-write-case ("org table sort is stable and honors explicit row bounds" ws)
    (put ws "sort.org"
         (format nil "# keep this comment~%#+FR-013-UNKNOWN: retained~%| Z | 3 |~%| A | second |~%| A | first |~%| 0 | outside |~%"))
    (let* ((before (file-octets ws "sort.org"))
           (sorted (org-ok ws '("org" "table" "sort" "sort.org" "--table" "1"
                                "--column" "1" "--type" "lexical" "--order" "ascending"
                                "--start" "1" "--end" "3")
                           "org.table.sort"))
           (expected
             (format nil "# keep this comment~%#+FR-013-UNKNOWN: retained~%| A | second |~%| A | first |~%| Z | 3 |~%| 0 | outside |~%")))
      (expect (jget sorted "table") :to-be 1)
      (expect (jget sorted "changed") :to-be t)
      (expect-org-write-and-undo ws "sort.org" before expected sorted))
    (put ws "bad-table.org" (format nil "| A | B |~%"))
    (let ((before (file-octets ws "bad-table.org")))
      (org-error ws '("org" "table" "align" "bad-table.org" "--table" "0")
                 "org.table.align" "argument.invalid")
      (org-error ws '("org" "table" "sort" "bad-table.org" "--table" "1"
                      "--column" "0")
                 "org.table.sort" "argument.invalid")
      (expect (file-octets ws "bad-table.org") :to-equalp before)))

  (define-org-table-write-case
    ("org table sort supports numeric descending and undoes exactly" ws)
    (put ws "numeric-sort.org"
         (format nil "# keep this comment~%#+FR-013-UNKNOWN: retained~%| 2 | two |~%| 10 | ten |~%"))
    (let* ((before (file-octets ws "numeric-sort.org"))
           (sorted (org-ok ws '("org" "table" "sort" "numeric-sort.org" "--table" "1"
                                "--column" "1" "--type" "numeric" "--order" "descending"
                                "--start" "1" "--end" "2")
                           "org.table.sort"))
           (expected
             (format nil "# keep this comment~%#+FR-013-UNKNOWN: retained~%| 10 | ten |~%| 2 | two |~%")))
      (expect (jget sorted "table") :to-be 1)
      (expect (jget sorted "changed") :to-be t)
      (expect-org-write-and-undo ws "numeric-sort.org" before expected sorted)))

  (define-org-table-write-case ("org table formula recalculate evaluates and rejects the documented subset" ws)
    (put ws "formula.org"
         (format nil "| Input | Total |~%|-------+-------|~%| 2 | 0 |~%#+TBLFM: @2$2=$1+1~%"))
    (let ((result (org-ok ws '("org" "table" "recalculate" "formula.org" "--table" "1")
                          "org.table.recalculate")))
      (expect (jget result "changed") :to-be t)
      (expect (file-text ws "formula.org")
              :to-equal (format nil "| Input | Total |~%|-------+-------|~%| 2 | 3 |~%#+TBLFM: @2$2=$1+1~%")))
    (dolist (fixture
             (list
              (list "unsupported.org"
                    (format nil "| A | B |~%|---+---|~%| 2 | 0 |~%#+TBLFM: @2$2=$1^2~%")
                    "input.unsupported-format")
              (list "reference.org"
                    (format nil "| A | B |~%|---+---|~%| 2 | 0 |~%#+TBLFM: @2$2=$9+1~%")
                    "input.syntax-error")
              (list "invalid-number.org"
                    (format nil "| A | B |~%|---+---|~%| 2 | 0 |~%#+TBLFM: @2$2=\"x\"+1~%")
                    "input.syntax-error")
              (list "invalid-value.org"
                    (format nil "| A | B |~%|---+---|~%| 2 | 0 |~%#+TBLFM: @2$2=1||2~%")
                    "input.syntax-error")
              (list "division-by-zero.org"
                    (format nil "| A | B |~%|---+---|~%| 2 | 0 |~%#+TBLFM: @2$2=1/0~%")
                    "input.syntax-error")
              (list "unsupported-mode.org"
                    (format nil "| A | B |~%|---+---|~%| x | 0 |~%#+TBLFM: @2$2=$1;u~%")
                    "input.unsupported-format")))
      (destructuring-bind (path source code) fixture
        (put ws path source)
        (let ((before (file-octets ws path)))
          (org-error ws (list "org" "table" "recalculate" path "--table" "1")
                     "org.table.recalculate" code)
          (expect (file-octets ws path) :to-equalp before)))))

  (define-org-table-write-case ("org table formulas preserve precedence and first-directive behavior" ws)
    (put ws "precedence.org"
         (format nil "| Input | Result |~%|-------+--------|~%| 12 | 0 |~%| 12 | 0 |~%#+TBLFM: @2$2=12/3*2::@3$2=12/3/2~%"))
    (let ((result (org-ok ws '("org" "table" "recalculate" "precedence.org" "--table" "1")
                          "org.table.recalculate")))
      (expect (jget result "changed") :to-be t)
      (expect (file-text ws "precedence.org")
              :to-equal (format nil "| Input | Result |~%|-------+--------|~%| 12 | 2 |~%| 12 | 2 |~%#+TBLFM: @2$2=12/3*2::@3$2=12/3/2~%")))
    (put ws "multiple.org"
         (format nil "| Input | Total |~%|-------+-------|~%| 2 | 0 |~%#+TBLFM: @2$2=$1+1~%#+TBLFM: @2$2=$1+2~%"))
    (let ((result (org-ok ws '("org" "table" "recalculate" "multiple.org" "--table" "1")
                          "org.table.recalculate")))
      (expect (jget result "changed") :to-be t)
      (expect (file-text ws "multiple.org")
              :to-equal (format nil "| Input | Total |~%|-------+-------|~%| 2 | 3 |~%#+TBLFM: @2$2=$1+1~%#+TBLFM: @2$2=$1+2~%"))))

  (define-org-table-write-case ("org table diff truncation exposes commit, dry-run, and tx results" ws)
    (let ((fixture (org-diff-cut-fixture)))
      (put ws "large.org" fixture)
      (let* ((committed
               (org-ok ws
                       (list "org" "table" "align" "large.org" "--table" "1")
                       "org.table.align"))
             (op-id (jget committed "op_id"))
             (change (jget committed "changes" 0)))
        (expect (jget change "diff_truncated") :to-be t)
        (expect (stringp op-id) :to-be t)
        (expect (jlist (jget committed "next_commands"))
                :to-equal (list (format nil "aitools diff --op ~A" op-id))))
      (put ws "large.org" fixture)
      (let ((before (file-octets ws "large.org"))
            (dry-run
              (org-ok ws
                      (list "org" "table" "align" "large.org" "--table" "1" "--dry-run")
                      "org.table.align")))
        (expect (jget dry-run "dry_run") :to-be t)
        (expect (jget dry-run "changes" 0 "diff_truncated") :to-be t)
        (expect (nth-value 1 (gethash "next_commands" dry-run)) :to-be nil)
        (expect (file-octets ws "large.org") :to-equalp before))
      (let* ((tx (jget (run-ok ws (list "tx" "begin")) "tx"))
             (before (file-octets ws "large.org"))
             (staged
               (org-ok ws
                       (list "org" "table" "align" "large.org" "--table" "1" "--tx" tx)
                       "org.table.align"))
             (next (jget staged "next_commands" 0))
             (prefix (format nil "aitools tx diff ~A --max-diff-lines " tx))
             (prefix-at (and (stringp next) (search prefix next)))
             (limit (and prefix-at
                         (parse-integer next
                                        :start (+ prefix-at (length prefix))
                                        :junk-allowed t))))
        (expect (jget staged "tx") :to-equal tx)
        (expect (jget staged "tx_op") :to-be 1)
        (expect (jget staged "changes" 0 "diff_truncated") :to-be t)
        (expect (and (integerp limit) (> limit 200)) :to-be t)
        (expect (and (integerp limit)
                     (string= next (format nil "~A~D" prefix limit)))
                :to-be t)
        (expect (file-octets ws "large.org") :to-equalp before)
        (let* ((diff (run-ok ws (list "tx" "diff" tx "--max-diff-lines"
                                      (princ-to-string limit))))
               (change (jget diff "changes" 0)))
          (expect (jget change "diff_truncated") :to-be nil))
        (run-ok ws (list "tx" "commit" tx))
        (expect (/= (length (file-octets ws "large.org")) (length before)) :to-be t)))))
