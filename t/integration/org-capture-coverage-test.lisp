(in-package #:aitools.edit.test)

;;; Org capture exercised in process: the adapter functions are called
;;; directly, and the flow and command through the dispatcher, so the CI
;;; coverage gate counts them (the e2e cases run a built binary).

(defun %capture-cov-engine (source)
  "An engine for SOURCE that carries a source name, as the write flow's parser does."
  (multiple-value-bind (document failure)
      (aitools.org.infrastructure:parse-org-octets
       (sb-ext:string-to-octets source :external-format :utf-8)
       :source-name "capture.org")
    (expect failure :to-be nil)
    (aitools.org.domain:org-document-engine-value document)))

(defun %capture-cov-options (&rest options)
  (append options
          (list :at "2026-11-21T09:00:00" :tz "UTC" :dst-policy "compatible")))

(defun %capture-cov-heading-path (&rest segments)
  (format nil "[~{~A~^,~}]"
          (loop for (title index) on segments by #'cddr
                collect (format nil "{\"title\":\"~A\",\"index\":~D}" title index))))

(defun %capture-cov-edit (engine options)
  "The three values of %ORG-CAPTURE-EDIT, with the updated document as text."
  (multiple-value-bind (updated fields failure)
      (aitools.org.infrastructure::%org-capture-edit engine options)
    (values (and updated (org-kit:document-text updated)) fields failure)))

(defun %capture-cov-failure-kind (engine options)
  "The failure kind of capturing OPTIONS into ENGINE, requiring that nothing was produced."
  (multiple-value-bind (text fields failure) (%capture-cov-edit engine options)
    (expect text :to-be nil)
    (expect fields :to-be nil)
    (expect failure :to-be-truthy)
    (aitools.org.domain:org-failure-kind failure)))

(defun %capture-cov-ok (root arguments)
  (multiple-value-bind (code envelope stream)
      (aitools.test.support:run-aitools
       root
       (append arguments (list "--at" "2026-11-21T09:00:00" "--tz" "UTC")))
    (expect code :to-be 0)
    (expect stream :to-be :stdout)
    (expect (aitools.test.support:envelope-value envelope "command")
            :to-equal "org capture")
    (expect (aitools.test.support:envelope-value envelope "status")
            :to-equal "ok")
    envelope))

(defun %capture-cov-error (root arguments error-code)
  (multiple-value-bind (code envelope stream)
      (aitools.test.support:run-aitools root arguments)
    (expect code :to-be (aitools.protocol.domain:error-code-exit-code error-code))
    (expect stream :to-be :stderr)
    (expect (aitools.test.support:envelope-value envelope "status")
            :to-equal "error")
    (expect (aitools.test.support:envelope-value envelope "error" "code")
            :to-equal error-code)
    envelope))

(defun %capture-cov-put (root path source)
  (aitools.test.support:write-text (concatenate 'string root "/" path) source))

(defun %capture-cov-text (root path)
  (aitools.test.support:read-text (concatenate 'string root "/" path)))

(describe "Org capture adapter: headline target"
  (it "captures under the duplicate heading chosen by --heading-path and keeps the body"
    (let* ((source (format nil "* Parent~%** Same~%first~%** Same~%second~%* End~%tail~%"))
           (body (format nil "#+BEGIN_SRC text~%hello~%#+END_SRC~%: kept~%"))
           (engine (%capture-cov-engine source)))
      (expect
       (%capture-cov-edit
        engine
        (%capture-cov-options
         :target "headline" :title "New" :body body
         :heading-path (%capture-cov-heading-path "Parent" 1 "Same" 2)))
       :to-equal
       (format nil "* Parent~%** Same~%first~%** Same~%second~%*** New~%~A* End~%tail~%" body))
      (expect
       (%capture-cov-edit
        engine
        (%capture-cov-options
         :target "headline" :title "New" :body "text"
         :heading-path (%capture-cov-heading-path "Parent" 1 "Same" 1)))
       :to-equal
       (format nil "* Parent~%** Same~%first~%*** New~%text~%** Same~%second~%* End~%tail~%"))
      (expect (org-kit:document-text engine) :to-equal source)))
  (it "captures under the one heading --match selects, with and without a body"
    (let ((engine (%capture-cov-engine (format nil "* MTG~%existing~%"))))
      (expect
       (%capture-cov-edit
        engine
        (%capture-cov-options :target "headline" :title "Meeting" :match "LEVEL=1"))
       :to-equal (format nil "* MTG~%existing~%** Meeting~%"))
      (expect
       (%capture-cov-edit
        engine
        (%capture-cov-options :target "headline" :title "Meeting" :body "notes"
                              :match "LEVEL=1"))
       :to-equal (format nil "* MTG~%existing~%** Meeting~%notes~%"))))
  (it "accepts a body whose text only resembles a heading"
    (let ((engine (%capture-cov-engine (format nil "* Top~%"))))
      (expect
       (%capture-cov-edit
        engine
        (%capture-cov-options :target "headline" :title "New" :body "*bold* text"
                              :match "LEVEL=1"))
       :to-equal (format nil "* Top~%** New~%*bold* text~%"))))
  (it "rejects a selector that is absent, doubled, empty, non-string, or malformed"
    (let ((engine (%capture-cov-engine (format nil "* One~%* Two~%"))))
      (dolist (options
               (list (list :target "headline" :title "New")
                     (list :target "headline" :title "New"
                           :match "LEVEL=1"
                           :heading-path (%capture-cov-heading-path "One" 1))
                     (list :target "headline" :title "New" :match "")
                     (list :target "headline" :title "New" :heading-path "")
                     (list :target "headline" :title "New" :heading-path "not json")
                     (list :target "headline" :title "New" :match 42)
                     (list :target "headline" :title "New" :heading-path 42)))
        (expect (%capture-cov-failure-kind engine (apply #'%capture-cov-options options))
                :to-be :argument))))
  (it "reports a selection that matches nothing or more than one heading"
    (let ((engine (%capture-cov-engine (format nil "* One~%* Two~%"))))
      (dolist (case (list (list (list :match "LEVEL=4") :selection-no-match)
                          (list (list :match "LEVEL=1") :selection-ambiguous)
                          (list (list :heading-path (%capture-cov-heading-path "Absent" 1))
                                :selection-no-match)
                          (list (list :heading-path (%capture-cov-heading-path "One" 2))
                                :selection-no-match)))
        (expect
         (%capture-cov-failure-kind
          engine
          (apply #'%capture-cov-options :target "headline" :title "New" (first case)))
         :to-be (second case)))))
  (it "maps an invalid --at, --tz, or --dst-policy to an argument failure"
    (let ((engine (%capture-cov-engine (format nil "* One~%"))))
      (dolist (override (list (list :at "2026-02-30T09:00:00")
                              (list :at "not a time")
                              (list :tz "")
                              (list :dst-policy "sideways")))
        (expect
         (%capture-cov-failure-kind
          engine
          (append override
                  (list :target "headline" :title "New" :match "LEVEL=1"
                        :at "2026-11-21T09:00:00" :tz "UTC" :dst-policy "compatible")))
         :to-be :argument)))))

(describe "Org capture adapter: datetree target"
  (it "creates the absent year, month, and day"
    (dolist (initial (list ""
                           (format nil "* 2026~%")
                           (format nil "* 2026~%** 2026-11 November~%")))
      (expect
       (%capture-cov-edit
        (%capture-cov-engine initial)
        (%capture-cov-options :target "datetree" :title "New" :body "Body"))
       :to-equal
       (format nil "* 2026~%** 2026-11 November~%*** 2026-11-21 Saturday~%**** New~%Body~%"))))
  (it "reuses an existing day"
    (expect
     (%capture-cov-edit
      (%capture-cov-engine
       (format nil "* 2026~%** 2026-11 November~%*** 2026-11-21 Saturday~%existing~%"))
      (%capture-cov-options :target "datetree" :title "New"))
     :to-equal
     (format nil "* 2026~%** 2026-11 November~%*** 2026-11-21 Saturday~%existing~%**** New~%")))
  (it "refuses a heading selector"
    (let ((engine (%capture-cov-engine (format nil "* 2026~%"))))
      (dolist (selector (list (list :match "LEVEL=1")
                              (list :heading-path (%capture-cov-heading-path "2026" 1))))
        (multiple-value-bind (text fields failure)
            (%capture-cov-edit
             engine
             (apply #'%capture-cov-options :target "datetree" :title "New" selector))
          (expect text :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
          (expect (search "datetree" (aitools.org.domain:org-failure-message failure))
                  :to-be-truthy))))))

(describe "Org capture adapter: argument validation"
  (it "judges a title and body invalid exactly when they cannot form one heading"
    (let ((engine (%capture-cov-engine (format nil "* Top~%"))))
      (dolist (case (list (list "Title" nil nil)
                          (list "Title" "" nil)
                          (list "Title" "plain text" nil)
                          (list "Title" (format nil "line one~%line two~%") nil)
                          (list "Title" (format nil "* Child~%") t)
                          (list "Title" (format nil "text~%** Deep~%") t)
                          (list "" nil t)
                          (list nil nil t)
                          (list 42 nil t)
                          (list (format nil "two~%lines") nil t)
                          (list (format nil "two~Cmore" #\Return) nil t)))
        (expect (and (aitools.org.infrastructure::%capture-body-invalid-p
                      engine (first case) (second case))
                     t)
                :to-be (third case)))))
  (it "lists headlines in document order and none for a body without headings"
    (flet ((titles (source)
             (mapcar (lambda (node) (org-kit:node-property node :raw-value ""))
                     (aitools.org.infrastructure::%capture-headlines
                      (%capture-cov-engine source)))))
      (expect (titles (format nil "* A~%** B~%* C~%text~%")) :to-equal '("A" "B" "C"))
      (expect (titles "just text") :to-be nil)))
  (it "refuses an unusable title or a body with headings before touching the document"
    (let* ((source (format nil "* Top~%"))
           (engine (%capture-cov-engine source)))
      (dolist (override (list (list :title "")
                              (list :title nil)
                              (list :title 42)
                              (list :title (format nil "a~%b"))
                              (list :title (format nil "a~Cb" #\Return))
                              (list :title "ok" :body (format nil "* Child~%"))))
        (dolist (target '("headline" "datetree"))
          (multiple-value-bind (text fields failure)
              (%capture-cov-edit
               engine
               (append override
                       (%capture-cov-options :target target :match "LEVEL=1")))
            (expect text :to-be nil)
            (expect fields :to-be nil)
            (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
            (expect (search "--title must be a nonempty line"
                            (aitools.org.domain:org-failure-message failure))
                    :to-be-truthy))))
      (expect (org-kit:document-text engine) :to-equal source)))
  (it "refuses a target that is neither headline nor datetree"
    (let ((engine (%capture-cov-engine (format nil "* Top~%"))))
      (dolist (target (list "table" "HEADLINE" "" nil))
        (expect
         (%capture-cov-failure-kind
          engine
          (%capture-cov-options :target target :title "New" :match "LEVEL=1"))
         :to-be :argument)))))
