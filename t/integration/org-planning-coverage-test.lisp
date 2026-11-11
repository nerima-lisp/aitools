(in-package #:aitools.edit.test)
(describe "Org property and planning adapter edits"
  (it "updates, removes, and validates properties"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Task~%:PROPERTIES:~%:OWNER: Ada~%:END:~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (expect
       (nth-value 2
        (aitools.org.infrastructure::%org-property-edit
         engine (append options (list :name "STATUS" :value "ready")) nil))
       :to-be nil)
      (expect
       (nth-value 2
        (aitools.org.infrastructure::%org-property-edit
         engine (append options (list :name "OWNER")) t))
       :to-be nil)
      (expect
       (nth-value 0
        (aitools.org.infrastructure::%org-property-edit
         engine (append options (list :name "MISSING")) t))
       :to-be engine)
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 2
         (aitools.org.infrastructure::%org-property-edit
          engine (append options (list :name "" :value "ready")) nil)))
       :to-be :argument))
    (let ((aitools.org.infrastructure::*org-kit-limits*
            (org-kit:make-limits :max-output-length 1)))
      (let* ((document (%org-coverage-document
                        (format nil "#+TODO: TODO | DONE~%* TODO Task~%")))
             (engine (aitools.org.domain:org-document-engine-value document))
             (options (list :match "/TODO" :name "OWNER" :value "Ada"
                            :at "2026-10-19T09:00:00" :tz "UTC"
                            :dst-policy "compatible")))
        (expect
         (aitools.org.domain:org-failure-kind
          (nth-value 2
           (aitools.org.infrastructure::%org-property-edit
            engine options nil)))
         :to-be :too-large))))
  (it "applies every planning kind and maps edit failures"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Release~%   Description~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (dolist (case (list (list "scheduled" "<2026-10-20>")
                          (list "deadline" "<2026-10-21>")
                          (list "closed" "[2026-10-22]")))
        (expect
         (nth-value 2
          (aitools.org.infrastructure::%org-planning-edit
           engine (append options (list :kind (first case)
                                        :timestamp (second case))) nil))
         :to-be nil))
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 2
         (aitools.org.infrastructure::%org-planning-edit
          engine (append options (list :kind "invalid")) nil)))
       :to-be :argument)
      (expect
       (nth-value 0
        (aitools.org.infrastructure::%org-planning-edit
         engine (append options (list :kind "deadline")) t))
       :to-be engine))
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Release~%   SCHEDULED: <2026-10-20> SCHEDULED: <2026-10-22>~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :kind "scheduled"
                          :timestamp "<2026-10-21>"
                          :at "2026-10-19T09:00:00" :tz "UTC"
                          :dst-policy "compatible")))
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 2
         (aitools.org.infrastructure::%org-planning-edit engine options nil)))
       :to-be :selection-ambiguous))
    (let ((aitools.org.infrastructure::*org-kit-limits*
            (org-kit:make-limits :max-output-length 1)))
      (let* ((document (%org-coverage-document
                        (format nil "#+TODO: TODO | DONE~%* TODO Release~%")))
             (engine (aitools.org.domain:org-document-engine-value document))
             (options (list :match "/TODO" :kind "scheduled"
                            :timestamp "<2026-10-21>"
                            :at "2026-10-19T09:00:00" :tz "UTC"
                            :dst-policy "compatible")))
        (expect
         (aitools.org.domain:org-failure-kind
          (nth-value 2
           (aitools.org.infrastructure::%org-planning-edit
            engine options nil)))
         :to-be :too-large)))))
(describe "Org planning kind and deletion edges"
  (it "recognizes each supported planning kind"
    (dolist (case (list (list "scheduled" :scheduled)
                        (list "deadline" :deadline)
                        (list "closed" :closed)))
      (expect (aitools.org.infrastructure::%org-kind-keyword (first case))
              :to-be (second case)))
    (dolist (kind (list nil 7 "invalid"))
      (expect (aitools.org.infrastructure::%org-kind-keyword kind)
              :to-be nil)))
  (it "treats deletion of an absent planning marker as unchanged"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Release~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :kind "scheduled"
                          :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-planning-edit engine options t)
        (expect updated :to-be engine)
        (expect (cdr (assoc "kind" fields :test (function string=)))
                :to-equal "scheduled")
        (expect failure :to-be nil))))
  (it "maps an invalid planning timestamp before editing"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Release~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :kind "scheduled"
                          :timestamp "tomorrow"
                          :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-planning-edit engine options nil)
        (expect updated :to-be nil)
        (expect fields :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :argument)))))


