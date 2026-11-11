(in-package #:aitools.edit.test)
(defmacro %with-org-agenda-error (condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function (quote org-kit:agenda))
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))
(describe "Org agenda adapter failure mapping"
  (it "normalizes future repeat options"
    (expect (aitools.org.infrastructure::%agenda-repeat-mode nil)
            :to-be :all)
    (expect (aitools.org.infrastructure::%agenda-repeat-mode "all")
            :to-be :all)
    (expect (aitools.org.infrastructure::%agenda-repeat-mode "next")
            :to-be :next)
    (expect (aitools.org.infrastructure::%agenda-repeat-mode "none")
            :to-be :none)
    (expect (aitools.org.infrastructure::%agenda-repeat-mode "sometimes")
            :to-be nil))
  (it "maps agenda engine conditions"
    (let ((document (%org-coverage-document "* TODO Work")))
      (flet ((expect-agenda-failure (condition expected-kind)
               (%with-org-agenda-error condition
                 (multiple-value-bind (agenda failure)
                     (aitools.org.infrastructure::agenda-org-document
                      document "2026-10-19" "2026-10-20"
                      "2026-10-19T09:00:00" "UTC" "compatible" "all")
                   (expect agenda :to-be nil)
                   (expect (aitools.org.domain:org-failure-kind failure)
                           :to-be expected-kind)))))
        (expect-agenda-failure
         (make-condition (quote org-kit:org-resource-limit-error)
                         :message "agenda limit" :limit-name :max-output-length
                         :limit 1 :actual 2)
         :too-large)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-agenda-error)
                         :reason :invalid-range :message "bad range")
         :argument)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-agenda-error)
                         :reason :invalid-option :message "bad option")
         :argument)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-agenda-error)
                         :reason :habit-not-todo :message "bad habit")
         :syntax)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-agenda-error)
                         :reason :habit-without-schedule :message "bad schedule")
         :syntax)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-agenda-error)
                         :reason :habit-without-repeater :message "bad repeater")
         :syntax)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-agenda-error)
                         :reason :unsupported-entry :message "unsupported entry")
         :syntax)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-agenda-error)
                         :reason :other :message "agenda failed")
         :internal)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-time-error)
                         :reason :invalid-time :message "bad time")
         :argument)
        (expect-agenda-failure
         (make-condition (quote cl-date-kit:cl-date-kit-error))
         :argument)
        (expect-agenda-failure
         (make-condition (quote org-kit:org-kit-error)
                         :message "agenda failed")
         :internal)))))
