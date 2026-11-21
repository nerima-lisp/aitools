(in-package #:aitools.e2e.test)

(defparameter +hp-lines+
  '("#+TODO: TODO | DONE"
    "* Uno: 開発"
    "** TODO 要件を確認する"
    "** TODO 実装する"
    "* Tres: 監視(Sentry)"
    "** TODO 要件を確認する"
    "** TODO 実装する"
    "** TODO 要件を確認する"
    "* Cuatro: 運用"
    "** TODO 要件を確認する"))

(defparameter +hp-tres "Tres: 監視(Sentry)")
(defparameter +hp-requirement "要件を確認する")

(defun hp-text (lines)
  (format nil "~{~A~%~}" lines))

(defun hp-edit (line-number &rest replacement)
  "The fixture text with the one-based LINE-NUMBER replaced by the REPLACEMENT lines."
  (hp-text (loop for line in +hp-lines+
                 for n from 1
                 append (if (= n line-number) replacement (list line)))))

(defun hp-json (&rest segments)
  "A heading_path JSON text built from alternating title and index values."
  (format nil "[~{~A~^,~}]"
          (loop for (title index) on segments by #'cddr
                collect (format nil "{\"title\":\"~A\",\"index\":~A}" title index))))

(defun hp-query (ws file match)
  (jlist (jget (org-ok ws (list "org" "query" file "--match" match
                                "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                       "org query")
               "matches")))

(defun hp-path-json (ws file parent title index)
  "The heading_path of the INDEX-th TITLE under PARENT, re-serialized from the org query envelope."
  (let ((match (find-if (lambda (match)
                          (equal (mapcar (lambda (part) (list (jget part "title") (jget part "index")))
                                         (jlist (jget match "heading_path")))
                                 (list (list parent 1) (list title index))))
                        (hp-query ws file "LEVEL=2"))))
    (expect match :to-be-truthy)
    (json-kit:stringify (jget match "heading_path"))))

(defun hp-removed-lines (diff)
  "The old-file line numbers of the lines a unified DIFF removes."
  (let ((old 0) (removed '()))
    (dolist (line (text-lines diff) (nreverse removed))
      (cond ((zerop (length line)))
            ((and (> (length line) 3) (string= "@@ -" line :end2 4))
             (setf old (parse-integer line :start 4 :junk-allowed t)))
            ((char= #\\ (char line 0)))
            ((char= #\+ (char line 0)))
            ((char= #\- (char line 0)) (push old removed) (incf old))
            (t (incf old))))))

(defun hp-todo-args (file selector &optional (state "DONE"))
  (org-write-time-args (append (list "org" "todo" "set" file) selector (list "--state" state))))

(defun hp-heading-path (json)
  (list "--heading-path" json))

(describe "aitools e2e org --heading-path selection"
  (define-org-case ("org todo set --heading-path changes only the addressed duplicate title" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let ((original (file-octets ws "hp.org")))
      (let* ((json (hp-path-json ws "hp.org" +hp-tres +hp-requirement 1))
             (result (org-ok ws (hp-todo-args "hp.org" (hp-heading-path json)) "org todo set")))
        (expect (jget result "previous_state") :to-equal "TODO")
        (expect (jget result "new_state") :to-equal "DONE")
        (expect (jget result "changed") :to-be t)
        (expect-org-write-and-undo ws "hp.org" original
                                   (hp-edit 6 "** DONE 要件を確認する") result))
      (let* ((json (hp-path-json ws "hp.org" +hp-tres +hp-requirement 2))
             (result (org-ok ws (hp-todo-args "hp.org" (hp-heading-path json)) "org todo set")))
        (expect (jget result "changed") :to-be t)
        (expect-org-write-and-undo ws "hp.org" original
                                   (hp-edit 8 "** DONE 要件を確認する") result))))

  (define-org-case ("org todo set --heading-path dry-run agrees with the real write" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let* ((original (file-octets ws "hp.org"))
           (json (hp-path-json ws "hp.org" +hp-tres +hp-requirement 2))
           (arguments (hp-todo-args "hp.org" (hp-heading-path json)))
           (dry-run (org-ok ws (append arguments '("--dry-run")) "org todo set")))
      (expect (jget dry-run "dry_run") :to-be t)
      (expect (nth-value 1 (gethash "op_id" dry-run)) :to-be nil)
      (expect (file-octets ws "hp.org") :to-equalp original)
      (let ((real (org-ok ws arguments "org todo set")))
        (dolist (key '("previous_state" "new_state" "changed"))
          (expect (jget real key) :to-equal (jget dry-run key)))
        (expect (jget dry-run "changes" 0 "diff") :to-be-truthy)
        (expect (jget real "changes" 0 "diff") :to-equal (jget dry-run "changes" 0 "diff"))
        (expect-org-write-and-undo ws "hp.org" original (hp-edit 8 "** DONE 要件を確認する") real))))

  (define-org-case ("org todo set --heading-path misses leave the file unchanged" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let ((before (file-octets ws "hp.org")))
      (dolist (json (list (hp-json +hp-tres 1 "存在しない" 1)
                          (hp-json "Dos" 1 +hp-requirement 1)
                          (hp-json +hp-tres 1 +hp-requirement 3)
                          (hp-json +hp-tres 2 +hp-requirement 1)
                          (hp-json +hp-requirement 1)
                          (hp-json +hp-tres 1 +hp-requirement 1 "実装する" 1)))
        (org-error ws (hp-todo-args "hp.org" (hp-heading-path json)) "org todo set"
                   "selection.no-match")
        (expect (file-octets ws "hp.org") :to-equalp before))))

  (define-org-case ("org todo set rejects malformed --heading-path values and selector combinations" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let ((before (file-octets ws "hp.org")))
      (dolist (json (list (hp-json +hp-tres 1 +hp-requirement 0)
                          (hp-json +hp-tres 1 +hp-requirement -1)
                          (hp-json +hp-tres 1 +hp-requirement "1.5")
                          (hp-json +hp-tres 1 +hp-requirement "\"1\"")
                          (hp-json +hp-tres 1 +hp-requirement "null")
                          "[{"
                          "not json"
                          ""
                          "{\"title\":\"Tres: 監視(Sentry)\",\"index\":1}"
                          "[]"
                          "[{\"title\":\"Tres: 監視(Sentry)\",\"index\":1,\"extra\":true}]"
                          "[{\"title\":\"Tres: 監視(Sentry)\"}]"
                          "[{\"index\":1}]"
                          "[{\"title\":\"Tres: 監視(Sentry)\",\"title\":\"x\",\"index\":1}]"
                          "[\"Tres: 監視(Sentry)\"]"))
        (org-error ws (hp-todo-args "hp.org" (hp-heading-path json)) "org todo set"
                   "argument.invalid")
        (expect (file-octets ws "hp.org") :to-equalp before))
      (org-error ws (hp-todo-args "hp.org"
                                  (append '("--match" "/TODO")
                                          (hp-heading-path (hp-json +hp-tres 1))))
                 "org todo set" "argument.invalid")
      (expect (file-octets ws "hp.org") :to-equalp before)
      (org-error ws (hp-todo-args "hp.org" '()) "org todo set" "argument.invalid")
      (expect (file-octets ws "hp.org") :to-equalp before)))

  (define-org-case ("org todo set counts an empty selector option as given" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let ((before (file-octets ws "hp.org")))
      (dolist (selector (list (append '("--match" "/TODO") (hp-heading-path ""))
                              (append '("--match" "") (hp-heading-path (hp-json +hp-tres 1)))
                              (append '("--match" "") (hp-heading-path ""))
                              (hp-heading-path "")
                              '("--match" "")))
        (org-error ws (hp-todo-args "hp.org" selector) "org todo set" "argument.invalid")
        (expect (file-octets ws "hp.org") :to-equalp before)
        (org-error ws (append (hp-todo-args "hp.org" selector) '("--dry-run"))
                   "org todo set" "argument.invalid")
        (expect (file-octets ws "hp.org") :to-equalp before))))

  (define-org-case ("org todo set --match still reports an ambiguous selection" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let ((before (file-octets ws "hp.org")))
      (org-error ws (hp-todo-args "hp.org" '("--match" "/TODO")) "org todo set"
                 "selection.ambiguous")
      (expect (file-octets ws "hp.org") :to-equalp before))
    (put ws "single.org" (format nil "#+TODO: TODO | DONE~%* TODO One~%* DONE Two~%"))
    (let ((result (org-ok ws (hp-todo-args "single.org" '("--match" "/TODO")) "org todo set")))
      (expect (jget result "changed") :to-be t)
      (expect (file-text ws "single.org")
              :to-equal (format nil "#+TODO: TODO | DONE~%* DONE One~%* DONE Two~%"))))

  (define-org-case ("org property, planning, and clock writes accept --heading-path" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let* ((original (file-octets ws "hp.org"))
           (second-requirement (hp-path-json ws "hp.org" +hp-tres +hp-requirement 2))
           (first-requirement (hp-path-json ws "hp.org" +hp-tres +hp-requirement 1))
           (implement (hp-path-json ws "hp.org" +hp-tres "実装する" 1))
           (property-text (hp-edit 8 "** TODO 要件を確認する" ":PROPERTIES:" ":OWNER: A" ":END:")))
      (let ((set (org-ok ws (org-write-time-args
                             (append (list "org" "property" "set" "hp.org")
                                     (hp-heading-path second-requirement)
                                     '("--name" "OWNER" "--value" "A")))
                         "org property set")))
        (expect (jget set "changed") :to-be t)
        (expect (file-octets ws "hp.org") :to-equalp (octets property-text))
        (let ((deleted (org-ok ws (org-write-time-args
                                   (append (list "org" "property" "delete" "hp.org")
                                           (hp-heading-path second-requirement)
                                           '("--name" "OWNER")))
                               "org property delete")))
          (expect (jget deleted "changed") :to-be t)
          (expect (file-octets ws "hp.org") :to-equalp original)))
      (let ((set (org-ok ws (org-write-time-args
                             (append (list "org" "planning" "set" "hp.org")
                                     (hp-heading-path first-requirement)
                                     '("--kind" "scheduled" "--timestamp" "<2026-10-20>")))
                         "org planning set")))
        (expect (jget set "changed") :to-be t)
        (expect (file-octets ws "hp.org")
                :to-equalp (octets (hp-edit 6 "** TODO 要件を確認する" "SCHEDULED: <2026-10-20>")))
        (let ((deleted (org-ok ws (org-write-time-args
                                   (append (list "org" "planning" "delete" "hp.org")
                                           (hp-heading-path first-requirement)
                                           '("--kind" "scheduled")))
                               "org planning delete")))
          (expect (jget deleted "changed") :to-be t)
          (expect (file-octets ws "hp.org") :to-equalp original)))
      (let ((clock-in (org-ok ws (org-write-time-args
                                  (append (list "org" "clock" "in" "hp.org")
                                          (hp-heading-path implement)))
                              "org clock in")))
        (expect (jget clock-in "timestamp") :to-equal "[2026-10-19 Mon 09:00]")
        (expect (file-octets ws "hp.org")
                :to-equalp (octets (hp-edit 7 "** TODO 実装する" ":LOGBOOK:"
                                            "CLOCK: [2026-10-19 Mon 09:00]" ":END:"))))
      (let ((clock-out (org-ok ws (org-write-time-args
                                   (append (list "org" "clock" "out" "hp.org")
                                           (hp-heading-path implement))
                                   "2026-10-19T09:30:00")
                               "org clock out")))
        (expect (jget clock-out "interval")
                :to-equal "[2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] => 0:30")
        (expect (file-octets ws "hp.org")
                :to-equalp (octets (hp-edit 7 "** TODO 実装する" ":LOGBOOK:"
                                            "CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30"
                                            ":END:"))))))

  (define-org-case ("org clock report --heading-path reads only the addressed heading" ws)
    (put ws "hp-clock.org"
         (hp-text (list "* Tres: 監視(Sentry)"
                        "** TODO 要件を確認する"
                        ":LOGBOOK:"
                        "CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30"
                        ":END:"
                        "** TODO 要件を確認する"
                        ":LOGBOOK:"
                        "CLOCK: [2026-10-19 Mon 10:00]--[2026-10-19 Mon 10:15] =>  0:15"
                        ":END:")))
    (let ((before (file-octets ws "hp-clock.org")))
      (loop for (index minutes) in '((1 30) (2 15))
            do (let ((report (org-ok ws (append
                                         (list "org" "clock" "report" "hp-clock.org")
                                         (hp-heading-path
                                          (hp-path-json ws "hp-clock.org" +hp-tres +hp-requirement index))
                                         '("--from" "2026-10-19T09:00:00" "--through" "2026-10-19T11:00:00"
                                           "--at" "2026-10-19T11:00:00" "--tz" "UTC"))
                                     "org clock report")))
                 (expect (jget report "total_minutes") :to-be minutes)
                 (expect (jget report "rows" 0 "title") :to-equal +hp-requirement)
                 (expect (file-octets ws "hp-clock.org") :to-equalp before)))
      (org-error ws (list "org" "clock" "report" "hp-clock.org"
                          "--heading-path" (hp-json +hp-tres 1 +hp-requirement 3)
                          "--from" "2026-10-19T09:00:00" "--through" "2026-10-19T11:00:00"
                          "--at" "2026-10-19T11:00:00" "--tz" "UTC")
                 "org clock report" "selection.no-match")))

  (define-org-case ("org query heading_path round-trips to the same heading for every match" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let* ((before (file-octets ws "hp.org"))
           (matches (hp-query ws "hp.org" "LEVEL=2")))
      (expect (length matches) :to-be 6)
      (dolist (match matches)
        (let ((dry-run (org-ok ws (append (hp-todo-args
                                           "hp.org"
                                           (hp-heading-path (json-kit:stringify (jget match "heading_path"))))
                                          '("--dry-run"))
                               "org todo set")))
          (expect (hp-removed-lines (jget dry-run "changes" 0 "diff"))
                  :to-equal (list (jget match "line")))
          (expect (file-octets ws "hp.org") :to-equalp before))))))

(defun archive-args (source destination selector)
  (org-write-time-args (append (list "org" "archive" source "--to" destination "--target-heading" "Archived") selector)))

(describe "aitools e2e org archive"
  (define-org-case ("archives selected duplicate subtree and preserves drawers" ws)
    (put ws "source.org" (hp-text '("#+TODO: TODO | DONE" "* Project" "** DONE Task" ":PROPERTIES:" ":OWNER: A" ":END:" ":LOGBOOK:" "- Note" ":END:" "*** Child" "** DONE Task" "*** Other")))
    (put ws "archive.org" "")
    (let* ((path (hp-path-json ws "source.org" "Project" "Task" 1))
           (args (archive-args "source.org" "archive.org" (hp-heading-path path)))
           (source (file-octets ws "source.org")) (dest (file-octets ws "archive.org"))
           (dry (org-ok ws (append args '("--dry-run")) "org archive")))
      (expect (file-octets ws "source.org") :to-equalp source)
      (expect (file-octets ws "archive.org") :to-equalp dest)
      (let ((real (org-ok ws args "org archive")))
        (expect (jget real "changed") :to-be t)
        (expect (jget real "changes" 0 "diff") :to-equal (jget dry "changes" 0 "diff"))
        (expect (jget real "changes" 1 "diff") :to-equal (jget dry "changes" 1 "diff"))
        (expect (file-text ws "source.org") :to-equal (hp-text '("#+TODO: TODO | DONE" "* Project" "** DONE Task" "*** Other")))
        (dolist (part '("Archived" "DONE Task" "Child" ":OWNER: A" ":LOGBOOK:" "ARCHIVE_TIME" "ARCHIVE_FILE" "ARCHIVE_OLPATH" "ARCHIVE_CATEGORY" "ARCHIVE_TODO"))
          (expect (search part (file-text ws "archive.org")) :to-be-truthy)))))
  (define-org-case ("creates missing archive and target for last heading" ws)
    (put ws "single.org" (hp-text '("#+TODO: TODO | DONE" "* DONE Only")))
    (org-ok ws (archive-args "single.org" "new.org" '("--match" "/DONE")) "org archive")
    (expect (file-text ws "single.org") :to-equal (hp-text '("#+TODO: TODO | DONE")))
    (expect (search "Only" (file-text ws "new.org")) :to-be-truthy)
    (expect (search "Archived" (file-text ws "new.org")) :to-be-truthy))
  (define-org-case ("appends under existing archive heading" ws)
    (put ws "single.org" (hp-text '("#+TODO: TODO | DONE" "* DONE Only")))
    (put ws "archive.org" (hp-text '("* Archived" "** Previous")))
    (org-ok ws (archive-args "single.org" "archive.org" '("--match" "/DONE")) "org archive")
    (expect (search "Previous" (file-text ws "archive.org")) :to-be-truthy)
    (expect (search "Only" (file-text ws "archive.org")) :to-be-truthy))
  (define-org-case ("selection failures preserve both files" ws)
    (put ws "source.org" (hp-text '("#+TODO: TODO | DONE" "* DONE One" "* DONE Two")))
    (put ws "archive.org" (hp-text '("* Archived")))
    (let ((source (file-octets ws "source.org")) (dest (file-octets ws "archive.org")))
      (org-error ws (archive-args "source.org" "archive.org" '("--match" "/TODO")) "org archive" "selection.no-match")
      (org-error ws (archive-args "source.org" "archive.org" '("--match" "/DONE")) "org archive" "selection.ambiguous")
      (expect (file-octets ws "source.org") :to-equalp source)
      (expect (file-octets ws "archive.org") :to-equalp dest)))
  (define-org-case ("moves within one file" ws)
    (put ws "same.org" (hp-text '("#+TODO: TODO | DONE" "* DONE Task" "** Child" "* Archived" "** Previous")))
    (org-ok ws (archive-args "same.org" "same.org" '("--match" "/DONE")) "org archive")
    (let ((text (file-text ws "same.org")))
      (expect (search "** DONE Task" text) :to-be-truthy)
      (expect (search "*** Child" text) :to-be-truthy)
      (expect (search "** Previous" text) :to-be-truthy)
      (expect (search "ARCHIVE_FILE" text) :to-be-truthy))))

(describe "aitools e2e org archive metadata and destination validation"
  (define-org-case ("records relative source and preserves duplicate children" ws)
    (ensure-directories-exist (ws-path ws "task/file.org"))
    (ensure-directories-exist (ws-path ws "archive/2026/file.org"))
    (put ws "task/file.org"
         (hp-text '("#+TODO: TODO | DONE" "* Project" "** DONE Task"
                    ":PROPERTIES:" ":OWNER: A" ":END:" ":LOGBOOK:" "- Note" ":END:"
                    "*** Same" "*** Same" "** DONE Other")))
    (put ws "archive/2026/file.org" "")
    (let ((path (hp-path-json ws "task/file.org" "Project" "Task" 1)))
      (org-ok ws (archive-args "task/file.org" "archive/2026/file.org" (hp-heading-path path)) "org archive"))
    (expect (file-text ws "task/file.org")
            :to-equal (hp-text '("#+TODO: TODO | DONE" "* Project" "** DONE Other")))
    (let ((text (file-text ws "archive/2026/file.org")))
      (dolist (part '(":ARCHIVE_TIME: [2026-10-19 Mon 09:00]"
                      ":ARCHIVE_FILE: ../../task/file.org" ":ARCHIVE_OLPATH: Project"
                      ":ARCHIVE_CATEGORY: file" ":ARCHIVE_TODO: DONE"))
        (expect (search part text) :to-be-truthy))
      (let ((first (search "*** Same" text)))
        (expect first :to-be-truthy)
        (expect (search "*** Same" text :start2 (+ first 8)) :to-be-truthy))))
  (define-org-case ("ambiguous destination leaves both files unchanged" ws)
    (put ws "source.org" (hp-text '("#+TODO: TODO | DONE" "* DONE Task")))
    (put ws "archive.org" (hp-text '("* Archived" "* Archived")))
    (let ((source (file-octets ws "source.org")) (dest (file-octets ws "archive.org")))
      (org-error ws (archive-args "source.org" "archive.org" '("--match" "/DONE"))
                 "org archive" "selection.ambiguous")
      (expect (file-octets ws "source.org") :to-equalp source)
      (expect (file-octets ws "archive.org") :to-equalp dest))))

(describe "aitools e2e org archive destination edit failure"
  (define-org-case ("incompatible destination workflow leaves both files unchanged" ws)
    (put ws "source.org" (hp-text '("#+TODO: TODO | DONE" "* DONE Task")))
    (put ws "archive.org" (hp-text '("#+TODO: TODO | FINISHED" "* Archived")))
    (let ((source (file-octets ws "source.org")) (dest (file-octets ws "archive.org")))
      (org-error ws (archive-args "source.org" "archive.org" '("--match" "/DONE"))
                 "org archive" "internal.unexpected")
      (expect (file-octets ws "source.org") :to-equalp source)
      (expect (file-octets ws "archive.org") :to-equalp dest))))

(describe "aitools e2e org archive nested destination"
  (define-org-case ("moves under the nested target heading" ws)
    (put ws "source.org" (hp-text '("#+TODO: TODO | DONE" "* DONE Task" "** Child")))
    (put ws "archive.org" (hp-text '("* Parent" "** Archived" "*** Existing" "* Sibling")))
    (org-ok ws (archive-args "source.org" "archive.org" '("--match" "/DONE")) "org archive")
    (let ((text (file-text ws "archive.org")))
      (expect (search "** Archived" text) :to-be-truthy)
      (expect (search "*** Existing" text) :to-be-truthy)
      (expect (search "*** DONE Task" text) :to-be-truthy)
      (expect (search "**** Child" text) :to-be-truthy)
      (expect (search "* Sibling" text) :to-be-truthy)
      (expect (< (search "** Archived" text) (search "*** DONE Task" text) (search "* Sibling" text)) :to-be t))))
