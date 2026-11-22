(in-package #:aitools.edit.test)

(describe "Org selector validation"
  (it "accepts exactly one non-empty selector"
    (expect (aitools.org.infrastructure::%selector-failure (list :match "/TODO"))
            :to-be nil)
    (expect (aitools.org.infrastructure::%selector-failure
             (list :heading-path (%hpc-json "Parent" 1)))
            :to-be nil))
  (it "rejects missing, doubled, and empty selectors with an argument failure"
    (dolist (case (list (list "both selectors"
                              (list :match "/TODO" :heading-path (%hpc-json "Parent" 1))
                              "--match and --heading-path are mutually exclusive")
                        (list "empty match beside a heading path"
                              (list :match "" :heading-path (%hpc-json "Parent" 1))
                              "--match and --heading-path are mutually exclusive")
                        (list "empty match beside an empty heading path"
                              (list :match "" :heading-path "")
                              "--match and --heading-path are mutually exclusive")
                        (list "neither selector" '()
                              "exactly one of --match and --heading-path is required")
                        (list "empty match" (list :match "")
                              "--match must be a non-empty string")
                        (list "non-string match" (list :match 7)
                              "--match must be a non-empty string")
                        (list "empty heading path" (list :heading-path "")
                              "--heading-path must be a non-empty string")
                        (list "non-string heading path" (list :heading-path 7)
                              "--heading-path must be a non-empty string")))
      (let ((failure (aitools.org.infrastructure::%selector-failure (second case))))
        (expect (list (first case)
                      (aitools.org.domain:org-failure-kind failure)
                      (aitools.org.domain:org-failure-message failure))
                :to-equal (list (first case) :argument (third case)))))))

(describe "Org --heading-path heading selection"
  (it "selects the headline a valid heading path names"
    (let ((engine (%hpc-engine)))
      (multiple-value-bind (heading failure)
          (aitools.org.infrastructure::%org-select-heading-node
           engine (list :heading-path (%hpc-json "Parent" 2 "Child" 1)) nil)
        (expect failure :to-be nil)
        (expect (org-kit:node-begin heading) :to-be (%hpc-line-begin 11)))))
  (it "prefers the heading path when a match is also present"
    (let ((engine (%hpc-engine)))
      (multiple-value-bind (heading failure)
          (aitools.org.infrastructure::%org-select-heading-node
           engine (list :match "/Other" :heading-path (%hpc-json "Tres" 1)) nil)
        (expect failure :to-be nil)
        (expect (org-kit:node-begin heading) :to-be (%hpc-line-begin 8)))))
  (it "maps an invalid heading path to an argument failure naming the reason"
    (let ((engine (%hpc-engine)))
      (dolist (case (list (list "not JSON" "not json" +hpc-reason-json+)
                          (list "not an array" "{}" +hpc-reason-array+)
                          (list "bad segment" "[{\"title\":\"A\",\"index\":0}]"
                                +hpc-reason-segment+)))
        (multiple-value-bind (heading failure)
            (aitools.org.infrastructure::%org-select-heading-node
             engine (list :heading-path (second case)) nil)
          (expect (list (first case) heading
                        (aitools.org.domain:org-failure-kind failure)
                        (aitools.org.domain:org-failure-message failure))
                  :to-equal (list (first case) nil :argument
                                  (format nil "--heading-path is invalid: ~A"
                                          (third case))))))))
  (it "maps an unresolvable heading path to a no-match failure naming the level"
    (let ((engine (%hpc-engine)))
      (dolist (case +hpc-miss-cases+)
        (multiple-value-bind (heading failure)
            (aitools.org.infrastructure::%org-select-heading-node
             engine (list :heading-path (second case)) nil)
          (expect (list (first case) heading
                        (aitools.org.domain:org-failure-kind failure)
                        (aitools.org.domain:org-failure-message failure))
                  :to-equal (list (first case) nil :selection-no-match
                                  (third case)))))))
  (it "validates the selector and time before selecting by heading path"
    (let ((engine (%hpc-engine))
          (time (list :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading
           engine (append (list :heading-path (%hpc-json "Parent" 1 "Other" 1)) time))
        (declare (ignore context))
        (expect failure :to-be nil)
        (expect at :to-be-truthy)
        (expect (org-kit:node-begin heading) :to-be (%hpc-line-begin 7)))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading
           engine (append (list :match "/Other" :heading-path (%hpc-json "Tres" 1)) time))
        (expect (list heading at context) :to-equal '(nil nil nil))
        (expect (aitools.org.domain:org-failure-message failure)
                :to-equal "--match and --heading-path are mutually exclusive"))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading
           engine (list :heading-path (%hpc-json "Missing" 1)
                        :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy "compatible"))
        (expect (list heading at context) :to-equal '(nil nil nil))
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :selection-no-match))))
  (it "edits only the headline the heading path names"
    (let* ((engine (%hpc-engine))
           (options (list :heading-path (%hpc-json "Parent" 2 "Child" 1)
                          :name "OWNER" :value "Ada"
                          :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-property-edit engine options nil)
        (declare (ignore fields))
        (expect failure :to-be nil)
        (expect (org-kit:document-text updated)
                :to-equal (format nil "~{~A~%~}"
                                  (append (subseq +hpc-lines+ 0 11)
                                          (list ":PROPERTIES:" ":OWNER: Ada" ":END:")
                                          (subseq +hpc-lines+ 11)))))
      (let ((missing (list* :heading-path (%hpc-json "Parent" 3)
                            (nthcdr 2 options))))
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%org-property-edit engine missing nil)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :selection-no-match))))))
