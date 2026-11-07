(in-package #:aitools.store.test)

(defun store-test-clock-ms (clock)
    (floor (* 1000 (cl-boundary-kit:clock-monotonic clock))
           internal-time-units-per-second))
  (defun advance-store-test-clock-ms (clock milliseconds)
    (cl-boundary-kit:advance-fake-clock
     clock 0 :monotonic-delta
     (* milliseconds (/ internal-time-units-per-second 1000))))
  (defun make-store-test-sleeper (clock on-sleep &key (oversleep-ms 0))
    (cl-boundary-kit:make-sleeper
     :sleep-fn
     (lambda (seconds)
       (let ((milliseconds (round (* seconds 1000))))
         (funcall on-sleep milliseconds)
         (advance-store-test-clock-ms clock (+ milliseconds oversleep-ms))))))
  (defun store-with-boundaries (store io &key
                                (clock (cl-boundary-kit:make-clock))
                                (sleeper (cl-boundary-kit:make-sleeper)))
    (make-store io
                (store-root store)
                (subseq (store-state-directory store)
                        0
                        (position #\/ (store-state-directory store) :from-end t))
                :clock clock
                :sleeper sleeper))

(defun legacy-lock-schedule (timeout-ms)
  (let ((remaining timeout-ms)
        (interval 5)
        (attempts 0)
        (delays nil))
    (loop
      (incf attempts)
      (when (<= remaining 0)
        (return (values attempts (nreverse delays))))
      (let ((delay (min interval remaining)))
        (push delay delays)
        (decf remaining delay))
      (setf interval (min 100 (* interval 2))))))

(describe "aitools.store lock resilience"
  (it "runs a nested workspace lock without acquiring the flock twice"
    (call-with-temp-store
     (lambda (plain)
       (let* ((base-boundaries (store-boundaries plain))
              (real-try-lock (store-operation base-boundaries :try-lock))
              (attempts 0)
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 base-boundaries
                 :try-lock
                 (lambda (path &rest keys)
                   (incf attempts)
                   (apply real-try-lock path keys))))
              (store (store-with-boundaries plain boundaries)))
         (expect
          (with-workspace-lock (store 1000)
            (with-workspace-lock (store 1000) :nested))
          :to-be :nested)
         (expect attempts :to-be 1))) :io nil))
  (it-each ((0 1 nil)
              (1 2 (1))
              (5 2 (5))
              (6 3 (5 1))
              (105 6 (5 10 20 40 30))
              (155 6 (5 10 20 40 80))
              (156 7 (5 10 20 40 80 1))
              (200 7 (5 10 20 40 80 45))
              (300 8 (5 10 20 40 80 100 45)))
      "matches the pre-kit lock retry schedule for ~D ms"
      (timeout-ms expected-attempts expected-delays)
      (multiple-value-bind (legacy-attempts legacy-delays)
          (legacy-lock-schedule timeout-ms)
        (expect legacy-attempts :to-be expected-attempts)
        (expect legacy-delays :to-equal expected-delays)
        (call-with-temp-store
         (lambda (plain)
           (let* ((base-boundaries (store-boundaries plain))
                  (clock (cl-boundary-kit:make-fake-clock :start 0 :monotonic-start 0))
                  (attempts 0)
                  (delays nil)
                  (sleeper
                    (make-store-test-sleeper
                     clock
                     (lambda (milliseconds) (push milliseconds delays))))
                  (boundaries
                    (cl-boundary-kit:boundary-context-with
                     base-boundaries
                     :try-lock
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (incf attempts)
                       nil)))
                  (store (store-with-boundaries plain boundaries :clock clock :sleeper sleeper)))
             (expect
              (with-workspace-lock (store timeout-ms :on-timeout :busy)
                :acquired)
              :to-be :busy)
             (expect attempts :to-be legacy-attempts)
             (expect (nreverse delays) :to-equal legacy-delays)))
         :io nil)))
    (it "caps the retry attempt count for large lock timeouts"
      (expect
       (aitools.store.application::%lock-retry-attempt-limit 1000000000)
       :to-be 10000005))
  (it "accepts acquisition on the deadline attempt"
  (call-with-temp-store
   (lambda (plain)
     (let* ((base-boundaries (store-boundaries plain))
            (real-try-lock (store-operation base-boundaries :try-lock))
            (real-unlock (store-operation base-boundaries :unlock))
            (clock (cl-boundary-kit:make-fake-clock :start 0 :monotonic-start 0))
            (attempts 0)
            (delays nil)
            (unlocked nil)
            (sleeper
              (make-store-test-sleeper
               clock
               (lambda (milliseconds) (push milliseconds delays))))
            (boundaries
              (cl-boundary-kit:boundary-context-with
               base-boundaries
               :try-lock
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf attempts)
                 (when (>= (store-test-clock-ms clock) 105) :lock-handle))
               :unlock
               (lambda (handle) (push handle unlocked))))
            (store (store-with-boundaries plain boundaries :clock clock :sleeper sleeper)))
       (expect
        (with-workspace-lock (store 105 :on-timeout :busy)
          :acquired)
        :to-be :acquired)
       (expect attempts :to-be 6)
       (expect (nreverse delays) :to-equal '(5 10 20 40 30))
       (expect unlocked :to-equal '(:lock-handle))))
   :io nil))
  (it "stops retrying after an overslept backoff"
  (call-with-temp-store
   (lambda (plain)
     (let* ((base-boundaries (store-boundaries plain))
            (clock (cl-boundary-kit:make-fake-clock :start 0 :monotonic-start 0))
            (attempts 0)
            (delays nil)
            (sleeper
              (make-store-test-sleeper
               clock
               (lambda (milliseconds) (push milliseconds delays))
               :oversleep-ms 200))
            (boundaries
              (cl-boundary-kit:boundary-context-with
               base-boundaries
               :try-lock
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf attempts)
                 nil)))
            (store (store-with-boundaries plain boundaries :clock clock :sleeper sleeper)))
       (expect
        (with-workspace-lock (store 105 :on-timeout :busy)
          :acquired)
        :to-be :busy)
       (expect attempts :to-be 2)
       (expect (nreverse delays) :to-equal '(5))))
   :io nil)))

(describe "aitools.store planner boundary cases"
  (it
    "rejects moving a directory inside itself and later targeting its old path"
    (let ((entries
           (list (cons "d" (directory-state)) (cons "d/child" "x"))))
      (expect
       (nth-value 1 (plan entries (list (move-request "d" "d/child"))))
       :to-equal
       "argument.invalid")
      (expect
       (nth-value 1
         (plan entries
           (list (move-request "d" "moved")
                 (delete-request "d/child"))))
       :to-equal
       "argument.invalid")))
  (it "rejects missing move sources and invalid symlink targets"
    (expect
     (nth-value 1 (plan '() (list (move-request "missing" "dest"))))
     :to-equal
     "input.not-found")
    (expect
     (nth-value 1 (plan '() (list (symlink-request "link" ""))))
     :to-equal
     "argument.invalid")
    (expect
     (nth-value 1
       (plan '()
         (list
          (symlink-request "link"
            (format nil
              "target~Cpath"
              (code-char 0))))))
     :to-equal
     "argument.invalid")))
