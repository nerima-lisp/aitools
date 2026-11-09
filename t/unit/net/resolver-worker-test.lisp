(in-package #:aitools.net.test)

(describe "aitools.net.infrastructure OS resolver"
    #+sb-thread
    (it "returns at its deadline without accumulating resolver workers"
      (let ((sb-ext:*exit-timeout* 0))
        (let* ((started (sb-thread:make-semaphore :count 0))
             (release (sb-thread:make-semaphore :count 0))
             (finished (sb-thread:make-semaphore :count 0))
             (released nil)
             (finished-p nil)
             (resolver
               (make-os-resolver
                :lookup-function
                (lambda (host)
                  (declare (ignore host))
                  (sb-thread:signal-semaphore started)
                  (sb-thread:wait-on-semaphore release)
                  (sb-thread:signal-semaphore finished)
                  nil)))
             (caller
               (sb-thread:make-thread
                (lambda ()
                  (handler-case
                      (progn (funcall resolver "slow.example" :timeout 0.05d0)
                             :returned)
                    (net-error (condition) (net-error-code condition)))))))
        (unwind-protect
             (progn
               (expect (sb-thread:wait-on-semaphore started :timeout 1d0)
                       :to-be-truthy)
               (expect (sb-thread:join-thread caller
                                              :timeout 1d0
                                              :default :still-waiting)
                       :to-equal "environment.timeout")
               (expect (sb-thread:wait-on-semaphore finished :timeout 0.001d0)
                       :to-be nil)
               (expect (resolver-error-code
                        (lambda ()
                          (funcall resolver "queued.example" :timeout 0.01d0)))
                       :to-equal "environment.timeout")
               (setf released t)
               (sb-thread:signal-semaphore release)
               (setf finished-p (sb-thread:wait-on-semaphore finished :timeout 1d0))
               (expect finished-p :to-be-truthy)
               (expect (sb-thread:wait-on-semaphore
                        aitools.net.infrastructure::*resolver-worker-semaphore*
                        :timeout 1d0)
                       :to-be-truthy)
               (sb-thread:signal-semaphore
                aitools.net.infrastructure::*resolver-worker-semaphore*)
               (let* ((attempts 0)
                      (failed-resolver
                        (make-os-resolver
                         :lookup-function (lambda (host) (declare (ignore host)) nil)
                         :thread-function
                         (lambda (function &key name)
                           (incf attempts)
                           (if (= attempts 1)
                               (error "fixture thread startup failure")
                             (sb-thread:make-thread function :name name)))))
                      (startup-error
                        (resolver-error-code
                         (lambda () (funcall failed-resolver "worker.example")))))
                 (expect startup-error :to-equal "environment.unavailable")
                 (expect (funcall failed-resolver "worker.example") :to-equal nil)
                 (expect attempts :to-be 2)
                 (let* ((clock-values (list 0d0 0d0 11d0))
                        (starts 0)
                        (deadline-resolver
                          (make-os-resolver
                           :clock-function (lambda () (or (pop clock-values) 11d0))
                           :lookup-function (lambda (host) (declare (ignore host)) nil)
                           :thread-function
                           (lambda (function &key name)
                             (declare (ignore function name))
                             (incf starts)
                             (error "resolver worker must not start")))))
                   (expect (resolver-error-code
                            (lambda ()
                              (funcall deadline-resolver
                                       "queued.example"
                                       :deadline 10d0)))
                           :to-equal "environment.timeout")
                   (expect starts :to-be 0))))
          (unless released
            (sb-thread:signal-semaphore release))
          (unless finished-p
            (sb-thread:wait-on-semaphore finished :timeout 1d0))))))
    #+sb-thread
(it "releases the resolver slot after a clock failure"
  (let ((sb-ext:*exit-timeout* 0))
    (let* ((clock-calls 0)
         (lookup-count 0)
         (resolver
           (make-os-resolver
            :clock-function
            (lambda ()
              (incf clock-calls)
              (if (= clock-calls 3)
                  (error "fixture clock failure")
                0d0))
            :lookup-function
            (lambda (host)
              (declare (ignore host))
              (incf lookup-count)
              nil)))
         (first-call-failed nil))
    (handler-case
        (funcall resolver "first.example" :timeout 2d0)
      (error () (setf first-call-failed t)))
    (expect first-call-failed :to-be t)
    (expect (funcall resolver "second.example" :timeout 2d0)
            :to-be nil)
    (expect lookup-count :to-be 1))))

    #+sb-thread
    (it "uses only the remaining deadline after starting the worker"
      (let ((exit-timeout-after-call nil))
        (let* ((clock-values (list 0d0 0d0 0d0 11d0))
               (started (sb-thread:make-semaphore :count 0))
               (release (sb-thread:make-semaphore :count 0))
               (finished (sb-thread:make-semaphore :count 0))
               (released nil)
               (finished-p nil)
               (resolver
                 (make-os-resolver
                  :clock-function (lambda () (or (pop clock-values) 11d0))
                  :lookup-function
                  (lambda (host)
                    (declare (ignore host))
                    (sb-thread:signal-semaphore started)
                    (sb-thread:wait-on-semaphore release)
                    (sb-thread:signal-semaphore finished)
                    nil)))
               (caller
                 (sb-thread:make-thread
                  (lambda ()
                    (let ((sb-ext:*exit-timeout* 60))
                      (let ((result
                              (handler-case
                                  (progn (funcall resolver "slow.example" :timeout 10d0)
                                         :returned)
                                (net-error (condition) (net-error-code condition)))))
                        (setf exit-timeout-after-call sb-ext:*exit-timeout*)
                        result))))))
          (unwind-protect
               (progn
                 (expect (sb-thread:wait-on-semaphore started :timeout 1d0)
                         :to-be-truthy)
                 (expect (sb-thread:join-thread caller
                                                :timeout 1d0
                                                :default :still-waiting)
                         :to-equal "environment.timeout")
                 (expect exit-timeout-after-call :to-be 0)
                 (expect (sb-thread:wait-on-semaphore finished :timeout 0.001d0)
                         :to-be nil)
                 (setf released t)
                 (sb-thread:signal-semaphore release)
                 (setf finished-p (sb-thread:wait-on-semaphore finished :timeout 1d0))
                 (expect finished-p :to-be-truthy))
            (unless released
              (sb-thread:signal-semaphore release))
            (unless finished-p
              (sb-thread:wait-on-semaphore finished :timeout 1d0))))))
)
