(in-package #:aitools.search.test)

(defparameter +allocation-child-ready-marker+ "@@aitools-search-allocation-ready@@")
(defparameter +allocation-child-go-line+ (format nil "GO~%"))
(defparameter +allocation-child-startup-timeout+ 300)
(defparameter +allocation-child-measurement-timeout+ 120)

(defun %allocation-ready-position (output)
  (loop with search-start = 0
        with marker = +allocation-child-ready-marker+
        for start = (search marker output :start2 search-start)
        while start
        for end = (+ start (length marker))
        when (and (or (zerop start)
                      (member (char output (1- start)) '(#\Newline #\Return)))
                  (or (= end (length output))
                      (member (char output end) '(#\Newline #\Return))))
        return start
        do (setf search-start (1+ start))))

(defun %drain-allocation-child-stream (stream buffer read-character)
  (loop for character = (funcall read-character stream nil :eof)
        do (cond ((null character) (return nil))
                 ((eq character :eof) (return t))
                 (t (vector-push-extend character buffer)))))

(defun %allocation-child-buffer-string (buffer)
  (coerce buffer 'string))

(defun %wait-for-allocation-child-ready
    (process stdout stderr
     &key (timeout +allocation-child-startup-timeout+)
       (wait-for-input #'process-kit:wait-for-input)
       (fd-of #'sb-sys:fd-stream-fd)
       (process-try-wait #'process-kit:process-try-wait)
       (read-character #'read-char-no-hang)
       (clock #'get-internal-real-time))
  (let* ((deadline (+ (funcall clock) (* timeout internal-time-units-per-second)))
         (stdout-fd (funcall fd-of stdout))
         (stderr-fd (funcall fd-of stderr))
         (stdout-buffer (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
         (stderr-buffer (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
         (stdout-eof nil)
         (stderr-eof nil))
    (flet ((startup-timeout ()
             (error "Search allocation child startup timed out waiting for READY after ~,3F seconds.~%stdout:~%~A~%stderr:~%~A"
                    timeout (%allocation-child-buffer-string stdout-buffer)
                    (%allocation-child-buffer-string stderr-buffer))))
      (loop
        (let ((output (%allocation-child-buffer-string stdout-buffer)))
          (when (%allocation-ready-position output)
            (when (<= (- deadline (funcall clock)) 0)
              (startup-timeout))
            (return (values output (%allocation-child-buffer-string stderr-buffer)))))
        (let* ((remaining-ticks (- deadline (funcall clock)))
               (remaining-seconds (/ (max 0 remaining-ticks)
                                     (float internal-time-units-per-second 1d0)))
               (active-fds (append (unless stdout-eof (list stdout-fd))
                                   (unless stderr-eof (list stderr-fd)))))
          (when (<= remaining-ticks 0)
            (startup-timeout))
          (let ((ready-fds (funcall wait-for-input active-fds :timeout remaining-seconds)))
            (when (and (not stdout-eof) (member stdout-fd ready-fds))
              (setf stdout-eof
                    (%drain-allocation-child-stream stdout stdout-buffer read-character)))
            (when (and (not stderr-eof) (member stderr-fd ready-fds))
              (setf stderr-eof
                    (%drain-allocation-child-stream stderr stderr-buffer read-character)))
            (let ((output (%allocation-child-buffer-string stdout-buffer)))
              (when (%allocation-ready-position output)
                (when (<= (- deadline (funcall clock)) 0)
                  (startup-timeout))
                (return (values output (%allocation-child-buffer-string stderr-buffer)))))
            (when (<= (- deadline (funcall clock)) 0)
              (startup-timeout))
            (when (funcall process-try-wait process)
              (error "Search allocation child setup failed: child exited before READY.~%stdout:~%~A~%stderr:~%~A"
                     (%allocation-child-buffer-string stdout-buffer)
                     (%allocation-child-buffer-string stderr-buffer)))
            (when (and stdout-eof stderr-eof)
              (error "Search allocation child setup failed: output closed before READY.~%stdout:~%~A~%stderr:~%~A"
                     (%allocation-child-buffer-string stdout-buffer)
                     (%allocation-child-buffer-string stderr-buffer))))))))
)

(defun %run-allocation-child-protocol
    (process wait-for-ready communicate
     &key (startup-timeout +allocation-child-startup-timeout+)
       (measurement-timeout +allocation-child-measurement-timeout+)
       (process-try-wait #'process-kit:process-try-wait)
       (close-process #'process-kit:close-process)
       (close-process-streams #'process-kit:close-process-streams)
       (result-stdout #'process-kit:process-result-stdout)
       (result-stderr #'process-kit:process-result-stderr)
       (result-exit-code #'process-kit:process-result-exit-code)
       (result-timed-out-p #'process-kit:process-result-timed-out-p))
  (unwind-protect
       (multiple-value-bind (prefix-stdout prefix-stderr)
           (funcall wait-for-ready process startup-timeout)
         (let* ((result (funcall communicate process +allocation-child-go-line+
                                 measurement-timeout))
                (stdout (concatenate 'string prefix-stdout (funcall result-stdout result)))
                (stderr (concatenate 'string prefix-stderr (funcall result-stderr result))))
           (unless (and (not (funcall result-timed-out-p result))
                        (eql (funcall result-exit-code result) 0))
             (error "Child search allocation SBCL failed (exit code: ~S, timed out: ~S).~%stdout:~%~A~%stderr:~%~A"
                    (funcall result-exit-code result)
                    (funcall result-timed-out-p result) stdout stderr))
           (values stdout stderr)))
    (unless (funcall process-try-wait process)
      (funcall close-process process :terminate t))
    (funcall close-process-streams process)))

(defstruct %allocation-protocol-test-result stdout stderr exit-code timed-out-p)
(defstruct %allocation-protocol-test-cleanup process-terminated-p streams-closed-p)

(defparameter *allocation-protocol-test-measurements*
  '(("flow-count" 1 2) ("flow-ignore-case" 1 2) ("flow-matches" 1 2)
    ("flow-needle" 1 2) ("flow-word" 1 2) ("matcher" 1 2) ("no-literal" 1 2)))

(defun %allocation-protocol-test-output ()
  (format nil "@@aitools-search-allocation-results@@~%~S~%"
          *allocation-protocol-test-measurements*))

(defun %allocation-protocol-test-waiter
    (stdout stderr poll clock exited-p
     &optional (read-character #'read-char-no-hang))
  (lambda (process timeout)
    (%wait-for-allocation-child-ready
     process stdout stderr :timeout timeout :wait-for-input poll
     :read-character read-character :clock clock
     :fd-of (lambda (stream) (if (eq stream stdout) 11 12))
     :process-try-wait (lambda (child) (declare (ignore child)) exited-p))))

(defun %run-allocation-protocol-test
    (wait-for-ready communicate cleanup
     &key (exited-p t) (startup-timeout 3) (measurement-timeout 1))
  (let ((process :test-process))
    (%run-allocation-child-protocol
     process wait-for-ready communicate
     :startup-timeout startup-timeout
     :measurement-timeout measurement-timeout
     :process-try-wait (lambda (child) (declare (ignore child)) exited-p)
     :close-process (lambda (child &key terminate)
                      (declare (ignore child))
                      (setf (%allocation-protocol-test-cleanup-process-terminated-p cleanup)
                            terminate))
     :close-process-streams (lambda (child)
                              (declare (ignore child))
                              (setf (%allocation-protocol-test-cleanup-streams-closed-p cleanup)
                                    t))
     :result-stdout #'%allocation-protocol-test-result-stdout
     :result-stderr #'%allocation-protocol-test-result-stderr
     :result-exit-code #'%allocation-protocol-test-result-exit-code
     :result-timed-out-p #'%allocation-protocol-test-result-timed-out-p)))

(defun %allocation-protocol-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) (princ-to-string condition))))

