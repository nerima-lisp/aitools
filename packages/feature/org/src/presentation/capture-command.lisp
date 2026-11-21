(in-package #:aitools.org.presentation)

(defun %capture-command (boundaries)
  (make-command
   :name "capture" :description (%summary "org.capture")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options)
                    (list (%value-option "target" "TARGET" "Target kind: headline or datetree."
                                         :required-p t :choices '("headline" "datetree"))
                          (%value-option "title" "TEXT" "New heading title." :required-p t)
                          (%value-option "body" "TEXT" "Optional heading body."))
                    (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-capture/k boundaries invocation
               (list :target (option-value invocation :target)
                     :title (option-value invocation :title)
                     :body (option-value invocation :body))))))
