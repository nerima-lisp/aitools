(in-package #:aitools.workspace.test)

(describe "workspace boundary adapter edge cases"
  (it "requires environment and working-directory objects"
    (expect (handler-case (progn (make-workspace-boundaries) nil)
              (error () t))
            :to-be-truthy)
    (expect (handler-case
                (progn (make-workspace-boundaries
                        :environment (cl-boundary-kit:make-test-environment))
                       nil)
              (error () t))
            :to-be-truthy))
  (it "expands the common operation macro"
    (expect
     (nth-value
      1
      (macroexpand-1
       '(aitools.workspace.application::define-workspace-operation
         sample-operation :workspace-stat (path) "Return a sample entry.")))
     :to-be-truthy))
  (it "uses the default inline ordered mapper"
    (let ((boundaries
            (make-workspace-boundaries
             :list-directory #'identity :stat #'identity :read-link #'identity
             :read-octets #'identity
             :environment (cl-boundary-kit:make-test-environment)
             :working-directory
             (cl-boundary-kit:make-working-directory :get-fn (lambda () "/"))
             :home-directory #'identity)))
      (expect
       (host-call-with-ordered-mapper
        boundaries (lambda (mapper) (funcall mapper #'1+ '(1 2))))
       :to-equal '(2 3))))
  (it "maps inline when the processor count is one"
    (let* ((scan-thread (cl-concurrent-kit:current-thread))
           (processor-count-calls 0)
           (processor-count
             #'aitools.workspace.infrastructure:processor-count))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.workspace.infrastructure:processor-count)
            (lambda ()
              (if (eq (cl-concurrent-kit:current-thread) scan-thread)
                  (progn (incf processor-count-calls) 1)
                  (funcall processor-count)))))
        (expect
         (aitools.workspace.infrastructure:call-with-ordered-mapper
          (lambda (mapper) (funcall mapper #'1+ '(1 2))))
         :to-equal '(2 3)))
      (expect processor-count-calls :to-be 1)))
  (it "returns NIL when opendir rejects a regular file"
      (aitools.test.support:call-with-temporary-directory
       (lambda (directory)
         (let ((path (concatenate (quote string) (namestring directory) "entry")))
           (with-open-file (stream (sb-ext:parse-native-namestring path)
                                   :direction :output)
             (write-string "entry" stream))
           (expect
            (multiple-value-list
             (aitools.workspace.infrastructure::%list-directory path))
            :to-equal (quote (nil nil)))))))
  (it "omits a directory entry when lstat fails"
      (aitools.test.support:call-with-temporary-directory
       (lambda (directory)
         (let ((path (concatenate (quote string) (namestring directory) "entry")))
           (with-open-file (stream (sb-ext:parse-native-namestring path)
                                   :direction :output)
             (write-string "entry" stream))
           (cl-weave:with-mocked-functions
            (((symbol-function (quote sb-posix:lstat))
              (lambda (pathname)
                (declare (ignore pathname))
                (error "mock lstat failure"))))
            (expect
             (multiple-value-list
              (aitools.workspace.infrastructure::%list-directory (namestring directory)))
             :to-equal (quote (nil t))))))))
  (it "returns NIL when fstat fails after opening a file"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (let ((path (concatenate 'string (namestring directory) "entry")))
         (with-open-file (stream (sb-ext:parse-native-namestring path)
                                 :direction :output)
           (write-string "entry" stream))
         (let* ((scan-thread (cl-concurrent-kit:current-thread))
                (fstat #'sb-posix:fstat))
           (cl-weave:with-mocked-functions
               (((symbol-function 'sb-posix:fstat)
                 (lambda (fd)
                   (if (eq (cl-concurrent-kit:current-thread) scan-thread)
                       (error "mock fstat failure")
                       (funcall fstat fd)))))
             (expect (aitools.workspace.infrastructure:read-regular-file-octets path)
                     :to-be nil)))))))
  (it "normalizes the default host home directory"
    (let* ((scan-thread (cl-concurrent-kit:current-thread))
           (user-home #'host-kit:user-home-directory))
      (cl-weave:with-mocked-functions
          (((symbol-function 'host-kit:user-home-directory)
            (lambda (&rest arguments)
              (if (eq (cl-concurrent-kit:current-thread) scan-thread)
                  (sb-ext:parse-native-namestring "/home/test/")
                  (apply user-home arguments)))))
        (expect (aitools.workspace.infrastructure::%home-directory)
                :to-equal "/home/test")))))
(describe "workspace bounded stream reads"
  (it "refuses an oversized stat before reading any bytes"
    (let ((read-bytes 0))
      (expect
       (multiple-value-list
        (aitools.workspace.infrastructure::%read-stream-octets
         nil 4 3
         :reader (lambda (buffer stream)
                   (declare (ignore buffer stream))
                   (incf read-bytes))))
       :to-equal '(nil :too-large))
      (expect read-bytes :to-be 0)))
  (it "stops a growing stream at exactly limit plus one byte"
    (let ((read-bytes 0))
      (expect
       (multiple-value-list
        (aitools.workspace.infrastructure::%read-stream-octets
         nil 3 3
         :reader (lambda (buffer stream)
                   (declare (ignore stream))
                   (incf read-bytes (length buffer))
                   (length buffer))))
       :to-equal '(nil :too-large))
      (expect read-bytes :to-be 4)))
  (it "accepts a stream exactly at its limit"
    (let ((read-bytes 0))
      (multiple-value-bind (octets status)
          (aitools.workspace.infrastructure::%read-stream-octets
           nil 3 3
           :reader (lambda (buffer stream)
                     (declare (ignore stream))
                     (dotimes (index 3)
                       (setf (aref buffer index) index))
                     (incf read-bytes 3)
                     3))
        (expect octets :to-equalp #(0 1 2))
        (expect status :to-be nil)
        (expect read-bytes :to-be 3)))))
