(in-package #:aitools.edit.test)

;;; Failure mapping and dispatcher cases for org capture; helpers live in
;;; org-capture-coverage-test.lisp, which loads first.

(describe "Org capture adapter: org-kit failures"
  (it "maps org-kit conditions from the capture step to failures on either target"
    (let ((engine (%capture-cov-engine (format nil "* Top~%"))))
      (dolist (target '("headline" "datetree"))
        (let ((options (apply #'%capture-cov-options :target target :title "New"
                              (and (string= target "headline") (list :match "LEVEL=1")))))
          (dolist (case
                    (list
                     (list (make-condition 'org-kit:org-resource-limit-error
                                           :message "capture limit"
                                           :limit-name :max-output-length
                                           :limit 1 :actual 2)
                           :too-large)
                     (list (make-condition 'org-kit:org-edit-error
                                           :message "rejected" :reason :invalid-value)
                           :argument)
                     (list (make-condition 'org-kit:org-edit-error
                                           :message "missing" :reason :target-not-found)
                           :not-found)
                     (list (make-condition 'org-kit:org-kit-error :message "capture failed")
                           :internal)))
            (%with-org-planning-adapter-error
                org-kit:capture-into-target
                (first case)
              (expect (%capture-cov-failure-kind engine options) :to-be (second case))))))))
  (it "does not reach the capture step when the body is rejected"
    (let ((engine (%capture-cov-engine (format nil "* Top~%"))))
      (%with-org-planning-adapter-error
          org-kit:capture-into-target
          (make-condition 'org-kit:org-kit-error :message "must not be called")
        (expect
         (%capture-cov-failure-kind
          engine
          (%capture-cov-options :target "datetree" :title "" :match "LEVEL=1"))
         :to-be :argument)))))

(describe "Org capture through the in-process dispatcher"
  (it "validates a dry run, then writes the same diff under the selected duplicate"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((before (format nil "#+TITLE: Test~%* Parent~%** Same~%first~%** Same~%second~%* End~%tail~%"))
             (body (format nil "#+BEGIN_SRC text~%hello~%#+END_SRC~%: hello~%"))
             (expected (format nil "#+TITLE: Test~%* Parent~%** Same~%first~%** Same~%second~%*** New~%~A* End~%tail~%" body))
             (arguments (list "org" "capture" "capture.org" "--target" "headline"
                              "--title" "New" "--body" body
                              "--heading-path" (%capture-cov-heading-path "Parent" 1 "Same" 2))))
        (%capture-cov-put root "capture.org" before)
        (let ((dry (%capture-cov-ok root (append arguments '("--dry-run")))))
          (expect (aitools.test.support:envelope-value dry "dry_run") :to-be t)
          (expect (%capture-cov-text root "capture.org") :to-equal before)
          (let ((write (%capture-cov-ok root arguments)))
            (expect (aitools.test.support:envelope-value write "changed") :to-be t)
            (expect (aitools.test.support:envelope-value write "changes" 0 "diff")
                    :to-equal
                    (aitools.test.support:envelope-value dry "changes" 0 "diff"))
            (expect (%capture-cov-text root "capture.org") :to-equal expected))))))
  (it "captures under a --match selection and into a datetree"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (%capture-cov-put root "match.org" (format nil "* MTG~%existing~%"))
      (%capture-cov-ok root '("org" "capture" "match.org" "--target" "headline"
                              "--title" "Meeting" "--match" "LEVEL=1"))
      (expect (%capture-cov-text root "match.org")
              :to-equal (format nil "* MTG~%existing~%** Meeting~%"))
      (%capture-cov-put root "date.org" "")
      (let ((envelope (%capture-cov-ok root '("org" "capture" "date.org" "--target" "datetree"
                                              "--title" "New" "--body" "Body"
                                              "--dry-run"))))
        (expect (aitools.test.support:envelope-value envelope "dry_run") :to-be t)
        (expect (%capture-cov-text root "date.org") :to-equal ""))
      (%capture-cov-ok root '("org" "capture" "date.org" "--target" "datetree"
                              "--title" "New" "--body" "Body"))
      (expect (%capture-cov-text root "date.org")
              :to-equal
              (format nil "* 2026~%** 2026-11 November~%*** 2026-11-21 Saturday~%**** New~%Body~%"))))
  (it "reports capture failures with their error codes and leaves the file unchanged"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let ((source (format nil "* One~%* Two~%")))
        (%capture-cov-put root "errors.org" source)
        (dolist (case
                  (list
                   (list '("--target" "headline" "--title" "New" "--match" "LEVEL=4")
                         "selection.no-match")
                   (list '("--target" "headline" "--title" "New" "--match" "LEVEL=1")
                         "selection.ambiguous")
                   (list '("--target" "headline" "--title" "New")
                         "argument.invalid")
                   (list '("--target" "datetree" "--title" "New" "--match" "LEVEL=1")
                         "argument.invalid")
                   (list (list "--target" "headline" "--title" "New" "--match" "LEVEL=1"
                               "--body" (format nil "* Child~%"))
                         "argument.invalid")
                   (list '("--target" "headline" "--title" "" "--match" "LEVEL=1")
                         "argument.invalid")
                   (list '("--target" "headline" "--title" "New" "--match" "LEVEL=1"
                           "--at" "2026-02-30T09:00:00" "--tz" "UTC")
                         "argument.invalid")))
          (let ((envelope
                  (%capture-cov-error
                   root
                   (append '("org" "capture" "errors.org") (first case)
                           (unless (member "--at" (first case) :test #'string=)
                             '("--at" "2026-11-21T09:00:00" "--tz" "UTC")))
                   (second case))))
            (expect (aitools.test.support:envelope-value envelope "command")
                    :to-equal "org capture"))
          (expect (%capture-cov-text root "errors.org") :to-equal source)))))
  (it "rejects a target outside the declared choices and a missing title at parse time"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (%capture-cov-put root "choice.org" (format nil "* One~%"))
      (%capture-cov-error
       root '("org" "capture" "choice.org" "--target" "table" "--title" "New"
              "--match" "LEVEL=1")
       "argument.invalid")
      (%capture-cov-error
       root '("org" "capture" "choice.org" "--target" "headline" "--match" "LEVEL=1")
       "argument.invalid")
      (expect (%capture-cov-text root "choice.org") :to-equal (format nil "* One~%")))))
