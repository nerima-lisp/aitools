;;;; t/e2e/edit-envelope-compatibility.lisp
(in-package #:aitools.e2e.test)

(describe "aitools edit e2e envelope compatibility"
          (it "matches the pre-CPS exact-edit envelope"
              (with-progress ("edit envelope compatibility")
                             (aitools-binary)
                             (call-with-workspace
                              (lambda (ws)
                                (put ws "a.txt" (format nil "one~%two~%"))
                                (let* ((envelope
                                        (run-ok ws
                                                (list "edit"
                                                      "a.txt"
                                                      "--old"
                                                      "two"
                                                      "--new"
                                                      "2")))
                                       (changes
                                        (jlist (jget envelope "changes")))
                                       (post-cps
                                        (list (jget envelope "schema_version")
                                              (jget envelope "status")
                                              (jget envelope "command")
                                              (jget envelope "strategy")
                                              (mapcar
                                               (lambda (change)
                                                 (list
                                                  (sort
                                                   (loop for key being the hash-keys of change
                                                         collect key)
                                                   (function string<))
                                                  (jget change "path")
                                                  (jget change "action")
                                                  (jget change "hash_before")
                                                  (jget change "hash_after")
                                                  (jget change "diff")))
                                               changes)
                                              (stringp (jget envelope "op_id"))
                                              (sort
                                               (loop for key being the hash-keys of envelope
                                                     collect key)
                                               (function string<))))
                                       (pre-cps
                                        (list 2
                                              "ok"
                                              "edit"
                                              "exact"
                                              (list
                                               (list
                                                (quote
                                                 ("action" "diff"
                                                           "hash_after"
                                                           "hash_before"
                                                           "path"))
                                                "a.txt"
                                                "modified"
                                                "c3f9c8c283a2b1f2f1896f27a01cbe3cddc0c9d93f752e4639035a0f5b36f6e8"
                                                "b8c083898d90038ced2e04df2f932eefa7d187080dee0d9942be12c156d95034"
                                                (format nil
                                                        "@@ -1,2 +1,2 @@~% one~%-two~%+2~%")))
                                              t
                                              (quote
                                               ("changes" "command"
                                                          "op_id"
                                                          "schema_version"
                                                          "status"
                                                          "strategy")))))
                                  (expect envelope
                                          :to-match-envelope
                                          "status"
                                          "ok"
                                          "command"
                                          "edit"
                                          "strategy"
                                          "exact")
                                  (expect post-cps :to-equal pre-cps)
                                  (expect (file-text ws "a.txt")
                                          :to-equal
                                          (format nil "one~%2~%"))))))))
