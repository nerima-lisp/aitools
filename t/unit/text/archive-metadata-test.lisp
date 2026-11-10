(in-package #:aitools.text.test)

(defun archive-metadata-test-path (length)
  (let ((remaining length)
        (component-lengths '()))
    (loop while (> remaining 255)
          do (push 255 component-lengths) (decf remaining 256))
    (push remaining component-lengths)
    (format nil
            "~{~A~^/~}"
            (mapcar
             (lambda (component-length)
               (make-string component-length :initial-element #\a))
             (nreverse component-lengths)))))

(defun archive-metadata-test-last-zip-central-offset (archive)
  (let* ((eocd (- (length archive) 22))
         (offset (aitools.text.domain::%u32le archive (+ eocd 16)))
         (count (aitools.text.domain::%u16le archive (+ eocd 10))))
    (loop repeat (1- count)
          do (incf offset
                   (+ 46
                      (aitools.text.domain::%u16le archive (+ offset 28))
                      (aitools.text.domain::%u16le archive (+ offset 30))
                      (aitools.text.domain::%u16le archive (+ offset 32)))))
    offset))

(defun archive-metadata-test-zip-overrun ()
  (multiple-value-bind (full-count remainder)
      (floor aitools.text.domain::+archive-max-path-metadata-bytes+
             aitools.text.domain::+archive-max-link-target-bytes+)
    (let* ((target
            (archive-metadata-test-path
             (1- aitools.text.domain::+archive-max-link-target-bytes+)))
           (member
            (make-archive-member :name "x" :kind :symlink :link-target target))
           (suffix
            (when (plusp remainder)
              (list
               (make-archive-member :name
                                    "x"
                                    :kind
                                    :symlink
                                    :link-target
                                    (archive-metadata-test-path (1- remainder))))))
           (over (make-archive-member :name "" :kind :symlink :link-target "x"))
           (archive
            (write-zip
             (append (make-list full-count :initial-element member)
                     suffix
                     (list over)))))
      (put-u16 archive
               (+ (archive-metadata-test-last-zip-central-offset archive) 10)
               12)
      archive)))

(defun archive-metadata-test-tar-overrun ()
  (multiple-value-bind (full-count remainder)
      (floor aitools.text.domain::+archive-max-path-metadata-bytes+
             aitools.text.domain::+archive-max-link-target-bytes+)
    (let* ((global-target
            (archive-metadata-test-path
             (1- aitools.text.domain::+archive-max-link-target-bytes+)))
           (global-header
            (tar-block "Global"
                       #\g
                       :data
                       (aitools.text.domain::%pax-record "linkpath"
                                                         global-target)))
           (full-links
            (loop repeat full-count
                  collect (tar-block "x" #\2)))
           (suffix
            (when (plusp remainder)
              (list
               (tar-block "PaxHeader"
                          #\x
                          :data
                          (aitools.text.domain::%pax-record "linkpath"
                                                            (archive-metadata-test-path
                                                             (1- remainder))))
               (tar-block "x" #\2))))
           (over
            (list
             (tar-block "PaxHeader"
                        #\x
                        :data
                        (aitools.text.domain::%pax-record "linkpath" "x"))
             (tar-block "" #\2))))
      (apply #'tar-of (append (list global-header) full-links suffix over)))))

(describe "aitools.text.domain archive path metadata limits"
          (it "accepts the exact metadata budget and rejects one byte more"
              (let ((limit
                     aitools.text.domain::+archive-max-path-metadata-bytes+))
                (expect
                 (aitools.text.domain::%archive-path-metadata-total (1- limit)
                                                                    1)
                 :to-be
                 limit)
                (signals archive-limit-exceeded
                         (aitools.text.domain::%archive-path-metadata-total
                          limit
                          1))
                (expect
                 (aitools.text.domain::%archive-utf8-byte-length "aé日本")
                 :to-be
                 (length (string-bytes "aé日本")))))
          (it
           "rejects ZIP metadata overflow before inflating the crossing target"
           (let ((archive (archive-metadata-test-zip-overrun)))
             (signals archive-limit-exceeded (read-zip-entries archive))))
          (it
           "rejects tar metadata overflow before retaining the crossing entry"
           (let ((archive (archive-metadata-test-tar-overrun)))
             (signals archive-limit-exceeded (read-tar-entries archive)))))
(describe "aitools.text.domain archive member constructor"
  (it "checks valid and invalid public member slot values"
    (let ((make-member (symbol-function (find-symbol "MAKE-ARCHIVE-MEMBER"
                                                    "AITOOLS.TEXT.TEST")))
          (invalid-kind (find-symbol "HARDLINK" "KEYWORD"))
          (invalid-mode (parse-integer "4096"))
          (invalid-mtime (parse-integer "-1")))
      (let ((member (funcall make-member :kind :symlink :mode #o7777 :mtime 0)))
        (expect (list (archive-member-kind member)
                      (archive-member-mode member)
                      (archive-member-mtime member))
                :to-equal (list :symlink #o7777 0)))
      (signals type-error (funcall make-member :kind invalid-kind))
      (signals type-error (funcall make-member :mode invalid-mode))
      (signals type-error (funcall make-member :mtime invalid-mtime)))))
