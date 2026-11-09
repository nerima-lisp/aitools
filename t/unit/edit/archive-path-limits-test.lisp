(in-package #:aitools.edit.test)

(describe "archive planner path limits"
          (it "enforces path, component, and depth limits in UTF-8 bytes"
              (flet ((path-with-lengths (lengths)
                       (format nil
                               "~{~A~^/~}"
                               (mapcar
                                (lambda (length)
                                  (make-string length :initial-element #\a))
                                lengths)))
                     (path-with-depth (depth)
                       (format nil
                               "~{~A~^/~}"
                               (make-list depth :initial-element "a"))))
                (let* ((max-path aitools.text.domain:+archive-max-path-bytes+)
                       (max-component
                        aitools.text.domain:+archive-max-component-bytes+)
                       (max-depth aitools.text.domain:+archive-max-depth+)
                       (three-byte (code-char #x6f22))
                       (four-byte (code-char #x1f600)))
                  (expect
                   (aitools.edit.domain::path-problem
                    (path-with-lengths
                     (make-list 16 :initial-element max-component))
                    max-path)
                   :to-be-falsy)
                  (expect
                   (aitools.edit.domain::path-problem
                    (path-with-lengths (make-list 17 :initial-element 240))
                    max-path)
                   :to-be
                   :path-too-long)
                  (expect
                   (aitools.edit.domain::path-problem
                    (concatenate 'string
                                 (make-string (- max-component 3)
                                              :initial-element
                                              #\a)
                                 (string three-byte))
                    max-path)
                   :to-be-falsy)
                  (expect
                   (aitools.edit.domain::path-problem
                    (concatenate 'string
                                 (make-string (- max-component 2)
                                              :initial-element
                                              #\a)
                                 (string three-byte))
                    max-path)
                   :to-be
                   :component-too-long)
                  (expect
                   (aitools.edit.domain::path-problem
                    (concatenate 'string
                                 (make-string (- max-component 4)
                                              :initial-element
                                              #\a)
                                 (string four-byte))
                    max-path)
                   :to-be-falsy)
                  (expect
                   (aitools.edit.domain::path-problem
                    (concatenate 'string
                                 (make-string (- max-component 3)
                                              :initial-element
                                              #\a)
                                 (string four-byte))
                    max-path)
                   :to-be
                   :component-too-long)
                  (expect
                   (aitools.edit.domain::path-problem
                    (path-with-depth max-depth)
                    5000)
                   :to-be-falsy)
                  (expect
                   (aitools.edit.domain::path-problem
                    (path-with-depth (1+ max-depth))
                    5000)
                   :to-be
                   :too-deep)
                  (expect
                   (aitools.edit.domain::path-problem
                    (path-with-depth (+ 2 max-depth))
                    5000)
                   :to-be
                   :too-deep))))
          (it
           "rechecks a symlink parent if the prefix view changes during planning"
           (let ((parent-lookups 0)
                 (octets
                  (aitools.text.domain:write-tar
                   (list
                    (aitools.text.domain:make-archive-member :name
                                                             "dir/link"
                                                             :kind
                                                             :symlink
                                                             :link-target
                                                             "target")))))
             (expect
              (refusal-code
               (lambda ()
                 (plan-archive-extraction octets
                                          :tar
                                          "out"
                                          :max-bytes
                                          100
                                          :max-entries
                                          1
                                          :lookup-kind
                                          (constantly :absent)
                                          :make-prefix-checker
                                          (lambda (archive-symlink-paths)
                                            (declare (ignore
                                                      archive-symlink-paths))
                                            (lambda
                                                (action &optional
                                                        cursor
                                                        component)
                                              (ecase action
                                                (:start nil)
                                                (:enter
                                                 (let ((next
                                                        (append cursor
                                                                (list component))))
                                                   (incf parent-lookups)
                                                   (values next
                                                           (if (and
                                                                (string=
                                                                 component
                                                                 "dir")
                                                                (=
                                                                 parent-lookups
                                                                 2))
                                                               :symlink
                                                               :clear))))
                                                (:parent
                                                 (if cursor
                                                     (values (butlast cursor)
                                                             :clear)
                                                     (values cursor :outside)))
                                                (:finish (values cursor :clear))))))))
              :to-equal
              "refusal.outside-workspace")
             (expect parent-lookups :to-be 2)))
          (it
           "maps a gzip member name over the reader limit to refusal.too-large"
           (let* ((max-path aitools.text.domain:+archive-max-path-bytes+)
                  (name (make-string max-path :initial-element #\a))
                  (overlong-name
                   (make-string (1+ max-path) :initial-element #\a))
                  (make-plan
                   (lambda (member-name)
                     (plan-archive-extraction
                      (aitools.text.domain:gzip-compress (bytes "x")
                                                         :name
                                                         member-name)
                      :gz
                      "out"
                      :archive-path
                      "member.gz"
                      :max-bytes
                      10
                      :max-entries
                      1
                      :lookup-kind
                      (constantly :absent)
                      :make-prefix-checker
                      (constantly nil)))))
             (expect
              (length
               (nth-value 0
                          (aitools.text.domain:gzip-member-header
                           (aitools.text.domain:gzip-compress (bytes "x")
                                                              :name
                                                              name))))
              :to-be
              max-path)
             (expect
              (refusal-code
               (lambda ()
                 (funcall make-plan overlong-name)))
              :to-equal
              "refusal.too-large")))
          (it "rejects archive steps with unsupported kinds"
              (expect
               (handler-case (progn
                               (aitools.edit.domain::make-extract-step
                                (gensym "INVALID-ARCHIVE-STEP-")
                                "out/file")
                               nil)
                 (type-error ()
                   t))
               :to-be
               t)))
