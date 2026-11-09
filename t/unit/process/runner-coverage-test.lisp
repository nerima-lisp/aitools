(in-package #:aitools.process.test)
(describe "process borrowed input edge coverage"
  (it "returns zero for an empty sequence"
    (let ((input
            (make-instance (quote aitools.process.infrastructure::%borrowed-binary-input)
                           :source (make-fake-binary-input #())
                           :process :fake)))
      (expect (sb-gray:stream-read-sequence input #()) :to-be 0)))
  (it "keeps a non-capture output destination for borrowed input"
    (let ((spawn-output nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote process-kit:spawn))
            (lambda (program arguments &rest options)
              (declare (ignore program arguments))
              (setf spawn-output (getf options :output))
              :process))
           ((symbol-function (quote process-kit:communicate))
            (lambda (process &rest options)
              (declare (ignore process options))
              :communicated))
           ((symbol-function (quote process-kit:process-try-wait))
            (lambda (process) (declare (ignore process)) t))
           ((symbol-function (quote process-kit:close-process-streams))
            (lambda (process) (declare (ignore process)))))
        (aitools.process.infrastructure::%run-borrowing-input
         (list "tool") 100 :null (make-string-input-stream "input") nil nil))
      (expect spawn-output :to-be :null)))
  #+linux
  (it "returns EOF after an idle pipe stops polling"
    (let ((read-fd nil) (write-fd nil) (source nil) (alive-checks 0))
      (multiple-value-setq (read-fd write-fd) (sb-posix:pipe))
      (setf source (sb-sys:make-fd-stream
                    read-fd :input t :element-type (quote (unsigned-byte 8)))
            read-fd nil)
      (unwind-protect
           (progn
             (cl-weave:with-mocked-functions
                 (((symbol-function (quote process-kit:process-alive-p))
                   (lambda (process)
                     (declare (ignore process))
                     (= (incf alive-checks) 1))))
               (let ((input
                       (make-instance
                        (quote aitools.process.infrastructure::%borrowed-binary-input)
                        :source source :process :fake)))
                 (expect (sb-gray:stream-read-byte input) :to-be :eof)))
             (expect alive-checks :to-be 2))
        (when source (close source))
        (when write-fd (sb-posix:close write-fd))
        (when read-fd (sb-posix:close read-fd))))))

(describe "aitools process borrowing runner"
  (it "launches with streams and closes an unfinished borrowed-input process"
    (let ((spawn-options nil) (terminated nil) (closed-streams nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'process-kit:spawn)
            (lambda (program arguments &rest options)
              (declare (ignore program arguments))
              (setf spawn-options options)
              :process))
           ((symbol-function 'process-kit:communicate)
            (lambda (process &rest options)
              (declare (ignore process options))
              :communicated))
           ((symbol-function 'process-kit:process-try-wait)
            (lambda (process) (declare (ignore process)) nil))
           ((symbol-function 'process-kit:close-process)
            (lambda (process &key terminate)
              (declare (ignore process))
              (setf terminated terminate)))
           ((symbol-function 'process-kit:close-process-streams)
            (lambda (process)
              (declare (ignore process))
              (setf closed-streams t))))
        (expect (aitools.process.infrastructure::%run-borrowing-input
                 '("tool") 100 :capture (make-fake-binary-input #(65)) nil nil)
                :to-be :communicated))
      (expect (getf spawn-options :input) :to-be :stream)
      (expect (getf spawn-options :output) :to-be :stream)
      (expect terminated :to-be t)
      (expect closed-streams :to-be t)))
  (it "returns process-kit results, launch failures, and stream delegation"
    (cl-weave:with-mocked-functions
        (((symbol-function 'process-kit:run)
          (lambda (program arguments &rest options)
            (declare (ignore program arguments options)) :result)))
      (expect (multiple-value-list
               (aitools.process.infrastructure::%run-with-output
                '("tool") 100 :capture :null nil nil))
              :to-equal '(:result :result)))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.process.infrastructure::%run-borrowing-input)
          (lambda (&rest arguments)
            (declare (ignore arguments)) :borrowed)))
      (expect (multiple-value-list
               (aitools.process.infrastructure::%run-with-output
                '("tool") 100 :capture (make-string-input-stream "input") nil nil))
              :to-equal '(:result :borrowed)))
    (cl-weave:with-mocked-functions
        (((symbol-function 'process-kit:run)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (error 'process-kit:process-launch-error
                   :program "missing" :arguments nil :directory nil :cause nil))))
      (multiple-value-bind (status message program)
          (aitools.process.infrastructure::%run-with-output
           '("missing") 100 :capture :null nil nil)
        (expect status :to-be :unavailable)
        (expect (search "missing" message) :to-be-truthy)
        (expect program :to-equal "missing")))
    (cl-weave:with-mocked-functions
        (((symbol-function 'process-kit:run)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (error 'process-kit:process-error))))
      (signals aitools.process.application:process-port-error
        (aitools.process.infrastructure::%run-with-output
         '("tool") 100 :capture :null nil nil)))))

(describe "aitools borrowed binary input stream"
  (it "reads available bytes, returns EOF after process exit, and fills sequences"
    (let* ((source (make-fake-binary-input #(65 66 67)))
           (input (make-instance 'aitools.process.infrastructure::%borrowed-binary-input
                                 :source source :process :fake))
           (sequence (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
      (cl-weave:with-mocked-functions
          (((symbol-function 'process-kit:process-alive-p)
            (lambda (process) (declare (ignore process)) t)))
        (expect (sb-gray:stream-read-byte input) :to-be 65)
        (expect (sb-gray:stream-read-sequence input sequence) :to-be 2)
        (expect (coerce sequence 'list) :to-equal '(66 67))))
    (let ((input (make-instance 'aitools.process.infrastructure::%borrowed-binary-input
                                :source (make-fake-binary-input #()) :process :fake))
          (sequence (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
      (cl-weave:with-mocked-functions
          (((symbol-function 'process-kit:process-alive-p)
            (lambda (process) (declare (ignore process)) nil)))
        (expect (sb-gray:stream-read-byte input) :to-be :eof)
        (expect (sb-gray:stream-read-sequence input sequence 1) :to-be 1)))))
