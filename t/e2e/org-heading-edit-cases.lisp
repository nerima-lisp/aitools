(in-package #:aitools.e2e.test)

(defun heading-edit-args (operation &rest arguments)
  (org-write-time-args (append (list "org") operation (list "edit.org") arguments)))

(defun heading-edit-check (ws initial arguments expected command)
  (put ws "edit.org" initial)
  (let* ((before (file-octets ws "edit.org"))
         (preview (org-ok ws (append arguments '("--dry-run")) command)))
    (expect (jget preview "dry_run") :to-be t)
    (expect (file-octets ws "edit.org") :to-equalp before)
    (let ((result (org-ok ws arguments command)))
      (expect (jget result "changed") :to-equal (jget preview "changed"))
      (expect (jget preview "changes" 0 "diff") :to-be-truthy)
      (expect (jget result "changes" 0 "diff") :to-equal (jget preview "changes" 0 "diff"))
      (expect (file-octets ws "edit.org") :to-equalp (octets expected)))))
(defun heading-checkbox-emacs-check (ws before target item fixed)
  (put ws "emacs.org" before)
  (let* ((expected
           (oracle ws '("emacs")
                   (format nil "emacs -Q --batch --eval '(progn (require (quote org)) (find-file \"emacs.org\") (org-mode) (goto-char (point-min)) (search-forward ~S) (beginning-of-line) (org-toggle-checkbox) (save-buffer))' && cat emacs.org"
                           target)
                   (octets fixed) :octets t))
         (arguments (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" item))
         (batch-input
           (with-output-to-string (stream)
             (json-kit:write-json
              (vector (coerce (append arguments '("--dry-run")) 'vector)
                      #("read" "edit.org")
                      (coerce arguments 'vector))
              stream))))
    (put ws "edit.org" before)
    (let* ((batch (run-ok ws '("batch" "--stdin") :stdin batch-input))
           (preview (jget batch "results" 0))
           (middle (jget batch "results" 1))
           (result (jget batch "results" 2)))
      (expect (jget preview "dry_run") :to-be t)
      (expect (octets (lines-text (jget middle "lines"))) :to-equalp (octets before))
      (expect (jget result "changed") :to-equal (jget preview "changed"))
      (expect (jget preview "changes" 0 "diff") :to-be-truthy)
      (expect (jget result "changes" 0 "diff") :to-equal (jget preview "changes" 0 "diff"))
      (expect (file-octets ws "edit.org") :to-equalp expected))))

(describe "aitools e2e org heading and checkbox edits"
  (define-org-case ("priority set and delete preserve other lines" ws)
    (let ((plain (format nil "#+TODO: TODO | DONE~%* TODO Target~%body~%* DONE Other~%"))
          (a (format nil "#+TODO: TODO | DONE~%* TODO [#A] Target~%body~%* DONE Other~%"))
          (b (format nil "#+TODO: TODO | DONE~%* TODO [#B] Target~%body~%* DONE Other~%")))
      (heading-edit-check ws plain
                          (heading-edit-args '("priority" "set") "--match" "/TODO" "--priority" "A")
                          a "org priority set")
      (heading-edit-check ws a
                          (heading-edit-args '("priority" "set") "--match" "/TODO" "--priority" "B")
                          b "org priority set")
      (heading-edit-check ws b
                          (heading-edit-args '("priority" "delete") "--match" "/TODO")
                          plain "org priority delete")))

  (define-org-case ("tags affect only the selected heading and preserve inherited tags" ws)
    (let* ((none (format nil "* Parent :inherited:~%** TODO Target~%body~%** TODO Other :local:~%"))
           (one (format nil "* Parent :inherited:~%** TODO Target :local:~%body~%** TODO Other :local:~%"))
           (two (format nil "* Parent :inherited:~%** TODO Target :local:new:~%body~%** TODO Other :local:~%"))
           (replaced (format nil "* Parent :inherited:~%** TODO Target :new:~%body~%** TODO Other :local:~%"))
           (path (hp-json "Parent" 1 "Target" 1)))
      (heading-edit-check ws none
                          (heading-edit-args '("tags" "add") "--heading-path" path "--tags" "local")
                          one "org tags add")
      (heading-edit-check ws one
                          (heading-edit-args '("tags" "add") "--heading-path" path "--tags" "local,new,new")
                          two "org tags add")
      (heading-edit-check ws two
                          (heading-edit-args '("tags" "remove") "--heading-path" path "--tags" "local,inherited")
                          replaced "org tags remove")
      (heading-edit-check ws two
                          (heading-edit-args '("tags" "replace") "--heading-path" path "--tags" "new")
                          replaced "org tags replace")))

  (define-org-case ("renaming retains TODO priority tags and statistics cookie" ws)
    (let ((before (format nil "#+TODO: TODO | DONE~%* TODO [#A] Old [1/3] :work:~%body~%* DONE Other~%"))
          (after (format nil "#+TODO: TODO | DONE~%* TODO [#A] New [1/3] :work:~%body~%* DONE Other~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("heading" "rename") "--match" "/TODO" "--title" "New")
                          after "org heading rename")))
  (define-org-case ("renaming with a new cookie replaces the old cookie" ws)
    (let ((before (format nil "#+TODO: TODO | DONE~%* TODO [#A] Old [1/3] :work:~%body~%* DONE Other~%"))
          (after (format nil "#+TODO: TODO | DONE~%* TODO [#A] New [50%] :work:~%body~%* DONE Other~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("heading" "rename") "--match" "/TODO" "--title" "New [50%]")
                          after "org heading rename")))
  (define-org-case ("checkbox heading path retains its selected heading" ws)
    (let ((before (format nil "* TODO Target [0/1]~%  - [ ] Item~%* TODO Other~%"))
          (after (format nil "* TODO Target [1/1]~%  - [X] Item~%* TODO Other~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("checkbox" "toggle") "--heading-path"
                                             (hp-json "Target [0/1]" 1) "--item" "1")
                          after "org checkbox toggle")))

  (define-org-case ("checkbox toggle updates counts and nested items" ws)
    (let ((unchecked (format nil "* TODO Target [1/3]~%  - [ ] First~%  - [X] Second~%  - [-] Third~%* DONE Other~%"))
          (checked (format nil "* TODO Target [2/3]~%  - [X] First~%  - [X] Second~%  - [-] Third~%* DONE Other~%"))
          (partial (format nil "* TODO Target [2/3]~%  - [ ] First~%  - [X] Second~%  - [X] Third~%* DONE Other~%"))
          (empty (format nil "* TODO Target [0/3]~%  - [ ] First~%  - [ ] Second~%  - [-] Third~%* DONE Other~%")))
      (dolist (case (list (list "1" checked) (list "2" empty) (list "3" partial)))
        (heading-edit-check ws unchecked
                            (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" (first case))
                            (second case) "org checkbox toggle")))
    (let ((before (format nil "* TODO Target [0%]~%  - [ ] Parent [0/1]~%    - [ ] Child~%** DONE Other~%  - [ ] Unrelated~%"))
          (after (format nil "* TODO Target [100%]~%  - [X] Parent [1/1]~%    - [X] Child~%** DONE Other~%  - [ ] Unrelated~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" "2")
                          after "org checkbox toggle"))
    (let ((before (format nil "* TODO Target~%  - [ ] Item~%"))
          (after (format nil "* TODO Target~%  - [X] Item~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" "1")
                          after "org checkbox toggle")))

  (define-org-case ("missing or ambiguous selections and invalid item preserve bytes" ws)
    (let ((before (format nil "#+TODO: TODO | DONE~%* TODO One~%  - [ ] Item~%* TODO Two~%")))
      (put ws "edit.org" before)
      (dolist (case (list (list (heading-edit-args '("priority" "set") "--match" "/DONE" "--priority" "A")
                               "org priority set" "selection.no-match")
                          (list (heading-edit-args '("tags" "add") "--match" "/TODO" "--tags" "x")
                               "org tags add" "selection.ambiguous")
                          (list (heading-edit-args '("heading" "rename") "--heading-path" (hp-json "Missing" 1) "--title" "New")
                               "org heading rename" "selection.no-match")
                          (list (heading-edit-args '("checkbox" "toggle") "--heading-path" (hp-json "One" 1) "--item" "2")
                               "org checkbox toggle" "selection.no-match")))
        (org-error ws (first case) (second case) (third case))
        (expect (file-octets ws "edit.org") :to-equalp (octets before))))))

(describe "aitools e2e org heading edit edge cases"
  (define-org-case ("percentage cookie changes when a checkbox changes" ws)
    (let ((before (format nil "* TODO Target [0%]~%  - [ ] One~%  - [ ] Two~%"))
          (after (format nil "* TODO Target [50%]~%  - [X] One~%  - [ ] Two~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" "1")
                          after "org checkbox toggle")))
  (define-org-case ("repeating a leaf toggle and selecting a parent" ws)
    (let* ((before (format nil "* TODO Target [0/1]~%  - [ ] Item~%"))
           (checked (format nil "* TODO Target [1/1]~%  - [X] Item~%"))
           (args (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" "1")))
      (put ws "edit.org" before)
      (org-ok ws (append args '("--dry-run")) "org checkbox toggle")
      (expect (file-octets ws "edit.org") :to-equalp (octets before))
      (org-ok ws args "org checkbox toggle")
      (expect (file-octets ws "edit.org") :to-equalp (octets checked))
      (org-ok ws args "org checkbox toggle")
      (expect (file-octets ws "edit.org") :to-equalp (octets before)))
    (let* ((before (format nil "* TODO Target [0/1]~%  - [ ] Parent [0/1]~%    - [ ] Child~%"))
           (args (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" "1")))
      (put ws "edit.org" before)
      (let ((result (org-ok ws args "org checkbox toggle")))
        (expect (jget result "changed") :to-be nil)
        (expect (file-octets ws "edit.org") :to-equalp (octets before)))))
  (define-org-case ("two-level parent matches Emacs" ws)
  (heading-checkbox-emacs-check ws (format nil "* TODO Target [0/1]~%  - [ ] Parent [0/1]~%    - [ ] Child~%") "Child" "2"
                               (format nil "* TODO Target [1/1]~%  - [X] Parent [1/1]~%    - [X] Child~%")))
  (define-org-case ("three-level parents match Emacs" ws)
  (heading-checkbox-emacs-check ws (format nil "* TODO Target [0/1]~%  - [ ] Outer [0/1]~%    - [ ] Inner [0/1]~%      - [ ] Child~%") "Child" "3"
                               (format nil "* TODO Target [1/1]~%  - [X] Outer [1/1]~%    - [X] Inner [1/1]~%      - [X] Child~%")))
  (define-org-case ("partially checked parents match Emacs" ws)
  (heading-checkbox-emacs-check ws (format nil "* TODO Target [0/1]~%  - [ ] Outer [0/1]~%    - [ ] Inner [0/2]~%      - [ ] One~%      - [ ] Two~%") "One" "3"
                               (format nil "* TODO Target [0/1]~%  - [-] Outer [0/1]~%    - [-] Inner [1/2]~%      - [X] One~%      - [ ] Two~%")))
  (define-org-case ("unchecked parent matches Emacs" ws)
  (heading-checkbox-emacs-check ws (format nil "* TODO Target [1/1]~%  - [X] Parent [1/1]~%    - [X] Child~%") "Child" "2"
                               (format nil "* TODO Target [0/1]~%  - [ ] Parent [0/1]~%    - [ ] Child~%")))
  (define-org-case ("toggling an independent item refreshes stale cookies" ws)
    (let ((before (format nil "* TODO Target [0/2]~%  - [ ] One~%  - [ ] Other [9/9]~%    - [ ] Other child~%"))
          (after (format nil "* TODO Target [1/2]~%  - [X] One~%  - [ ] Other [0/1]~%    - [ ] Other child~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("checkbox" "toggle") "--match" "/TODO" "--item" "1")
                          after "org checkbox toggle")))

  (define-org-case ("removing a duplicated local tag leaves inherited tag intact" ws)
    (let ((before (format nil "* Parent :inherited:~%** TODO Target :dup:dup:keep:~%"))
          (after (format nil "* Parent :inherited:~%** TODO Target :keep:~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("tags" "remove") "--heading-path"
                                             (hp-json "Parent" 1 "Target" 1) "--tags" "dup")
                          after "org tags remove")))

  (define-org-case ("heading path selects one duplicate when changing priority" ws)
    (let* ((before (format nil "* Parent~%** TODO Same~%** TODO Same~%"))
           (after (format nil "* Parent~%** TODO Same~%** TODO [#A] Same~%")))
      (heading-edit-check ws before
                          (heading-edit-args '("priority" "set") "--heading-path"
                                             (hp-json "Parent" 1 "Same" 2) "--priority" "A")
                          after "org priority set"))))
