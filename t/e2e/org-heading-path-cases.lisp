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
              :to-equal (format nil "#+TODO: TODO | DONE~%* DONE One~%* DONE Two~%")))))
