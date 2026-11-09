(in-package #:aitools.edit.test)

(describe "archive PAX path limits"
          (it
           "maps raw PAX path and linkpath limit failures to refusal.too-large"
           (call-with-edit-workspace
            (lambda ()
              (let* ((max-path aitools.text.domain:+archive-max-path-bytes+)
                     (max-link-target
                      aitools.text.domain:+archive-max-link-target-bytes+)
                     (overlong-path
                      (format nil
                              "~{~A~^/~}"
                              (make-list 17
                                         :initial-element
                                         (make-string 240 :initial-element #\a))))
                     (overlong-link-target
                      (make-string (1+ max-link-target) :initial-element #\a))
                     (cases
                      (list (list "path" overlong-path "file" #\0)
                            (list "linkpath" overlong-link-target "link" #\2))))
                (labels ((raw-pax-tar (key value member-name member-type)
                           (let* ((record
                                   (aitools.text.domain::%pax-record key value))
                                  (pax-blocks
                                   (* 512 (ceiling (length record) 512)))
                                  (pax-header
                                   (aitools.text.domain::%tar-header
                                    (bytes "PaxHeader/entry")
                                    #()
                                    #()
                                    (length record)
                                    #o644
                                    0
                                    #\x))
                                  (member-header
                                   (aitools.text.domain::%tar-header
                                    (bytes member-name)
                                    #()
                                    (bytes "target")
                                    0
                                    #o644
                                    0
                                    member-type))
                                  (archive
                                   (make-array (+ 512 pax-blocks 512 1024)
                                               :element-type
                                               (quote (unsigned-byte 8))
                                               :initial-element
                                               0)))
                             (replace archive pax-header)
                             (replace archive record :start1 512)
                             (replace archive
                                      member-header
                                      :start1
                                      (+ 512 pax-blocks))
                             archive)))
                  (expect (length (bytes overlong-path)) :to-be (1+ max-path))
                  (expect (length (bytes overlong-link-target))
                          :to-be
                          (1+ max-link-target))
                  (dolist
                      (case cases)
                    (destructuring-bind (key value member-name member-type) case
                      (let ((output-path (format nil "out/~A" member-name)))
                        (put "archive.tar"
                             (raw-pax-tar key value member-name member-type))
                        (multiple-value-bind (status code message keys)
                            (run "archive.extract"
                                 (list "archive.tar")
                                 :to
                                 "out")
                          (declare (ignore message keys))
                          (expect (list key status code (kind output-path))
                                  :to-equal
                                  (list key :error "refusal.too-large" :absent))))))))))))
