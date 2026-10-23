(in-package #:aitools/test)

(describe "aitools CLI trace context"
          (it "projects monotonic spans with stable IDs, remapped parents, and registry metrics"
              (let* ((clock (cl-boundary-kit:make-fake-clock
                             :start 1700000000000
                             :monotonic-start 200))
                     (id-sequence 0)
                     (session
                      (aitools/cli::%make-trace-session
                       :clock clock
                       :id-generator (lambda (length)
                                       (format nil "~v,'0X" length (incf id-sequence)))
                       :monotonic-units-per-second 3))
                     (meta nil)
                     (child-values nil)
                     (result-values nil))
                (let ((aitools/cli::*trace-session* session)
                      (aitools/cli::*trace-envelope-owner-p* t))
                  (setf result-values
                        (multiple-value-list
                         (aitools/cli::%call-with-trace-span
                          :command
                          "read"
                          (lambda ()
                            (observability-kit:span-set-attribute
                             observability-kit::*current-span*
                             "dynamic.note"
                             "must-not-appear")
                            (setf child-values
                                  (multiple-value-list
                                   (aitools/cli::%call-with-trace-span
                                    :io
                                    "store.read-file"
                                    (lambda ()
                                      (aitools/cli::%call-with-trace-span
                                       :io "store.lstat" (lambda () :stat))
                                      (cl-boundary-kit:advance-fake-clock
                                       clock 0 :monotonic-delta 1)
                                      (values :read-result :secondary)))))
                            (aitools/cli::%call-with-trace-span
                             :io "store.write-file" (lambda () :write-result))
                            (cl-boundary-kit:advance-fake-clock
                             clock 0 :monotonic-delta 2)
                            (setf meta (aitools/cli::%trace-envelope-meta :partial))
                            (values 3 :payload))
                          :outcome-function
                          (lambda (values)
                            (if (eql (first values) 3) :partial :error))))))
                (let* ((trace (aitools.test.support:json-alist-value meta "trace"))
                       (spans (aitools.test.support:json-alist-value trace "spans"))
                       (metrics (aitools.test.support:json-alist-value trace "metrics"))
                       (span-keys
                        '("span_id" "parent_span_id" "kind" "name"
                                    "start_ns" "duration_ns" "outcome")))
                  (expect result-values :to-equal '(3 :payload))
                  (expect child-values :to-equal '(:read-result :secondary))
                  (expect (mapcar (lambda (span)
                                    (aitools.test.support:json-alist-value span "span_id"))
                                  spans)
                          :to-equal '(0 1 2 3))
                  (expect (mapcar (lambda (span)
                                    (aitools.test.support:json-alist-value span "parent_span_id"))
                                  spans)
                          :to-equal (list json-kit:+json-null+ 0 1 0))
                  (expect (mapcar (lambda (span)
                                    (aitools.test.support:json-alist-value span "kind"))
                                  spans)
                          :to-equal '("command" "io" "io" "io"))
                  (expect (mapcar (lambda (span)
                                    (aitools.test.support:json-alist-value span "start_ns"))
                                  spans)
                          :to-equal '(0 0 0 333333333))
                  (expect (mapcar (lambda (span)
                                    (aitools.test.support:json-alist-value span "duration_ns"))
                                  spans)
                          :to-equal '(1000000000 333333333 0 0))
                  (expect (mapcar (lambda (span)
                                    (aitools.test.support:json-alist-value span "outcome"))
                                  spans)
                          :to-equal '("partial" "ok" "ok" "ok"))
                  (expect (every (lambda (span)
                                   (equal (mapcar #'car
                                                  (aitools.test.support:json-alist span))
                                          span-keys))
                                 spans)
                          :to-be-truthy)
                  (expect (mapcar #'car (aitools.test.support:json-alist metrics))
                          :to-equal
                          '("span_count" "command_count" "child_process_count"
                                         "io_count" "duration_ns"))
                  (expect (mapcar (lambda (key)
                                    (aitools.test.support:json-alist-value metrics key))
                                  '("span_count" "command_count" "child_process_count"
                                                 "io_count" "duration_ns"))
                          :to-equal '(4 1 0 3 1000000000)))))

          (it "propagates the same condition and records error without its message"
              (let* ((session (aitools/cli::%make-trace-session))
                     (raised (make-condition 'simple-error
                                             :format-control "trace failure"
                                             :format-arguments nil))
                     (caught nil))
                (let ((aitools/cli::*trace-session* session))
                  (setf caught
                        (handler-case
                            (aitools/cli::%call-with-trace-span
                             :io "store.read-file" (lambda () (error raised)))
                          (simple-error (condition) condition))))
                (let* ((record (first (aitools/cli::%trace-session-records session)))
                       (span-id (observability-kit:span-record-span-id record)))
                  (expect caught :to-be raised)
                  (expect (observability-kit:span-record-status record) :to-be :error)
                  (expect (observability-kit:span-record-status-message record)
                          :to-be-falsy)
                  (expect (observability-kit:span-record-attributes record) :to-be-falsy)
                  (expect (gethash span-id (aitools/cli::%trace-session-outcomes session))
                          :to-be :error))))

          (it "keeps the disabled command path direct and preserves multiple values"
              (let ((aitools/cli::*trace-session* nil)
                    (aitools/cli::*trace-envelope-owner-p* nil)
                    (calls 0))
                (let ((values
                       (multiple-value-list
                        (aitools/cli::%call-with-command-trace
                         "read" nil
                         (lambda ()
                           (incf calls)
                           (values 0 :payload))))))
                  (expect values :to-equal '(0 :payload))
                  (expect calls :to-be 1)
                  (expect (aitools/cli::%trace-envelope-meta :ok) :to-be-falsy)
                  (expect aitools/cli::*trace-session* :to-be-falsy)))))

