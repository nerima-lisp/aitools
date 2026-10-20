(in-package #:aitools.store.test)

(defun store-with-io (store io)
  (make-store io
              (store-root store)
              (subseq (store-state-directory store)
                      0
                      (position #\/ (store-state-directory store) :from-end t))))

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
    (with-temp-store (plain)
      (let* ((base-io (store-io-port plain))
             (real-try-lock (store-io-try-lock base-io))
             (attempts 0)
             (io (aitools.store.application::copy-store-io
                  base-io
                  :try-lock (lambda (path &rest keys)
                              (incf attempts)
                              (apply real-try-lock path keys))))
             (store (store-with-io plain io)))
        (expect
         (with-workspace-lock (store 1000)
           (with-workspace-lock (store 1000) :nested))
         :to-be :nested)
        (expect attempts :to-be 1))))
  (it "matches the pre-kit lock retry schedule"
    (dolist (case '((0 1 nil)
                    (1 2 (1))
                    (5 2 (5))
                    (6 3 (5 1))
                    (105 6 (5 10 20 40 30))
                    (155 6 (5 10 20 40 80))
                    (156 7 (5 10 20 40 80 1))
                    (200 7 (5 10 20 40 80 45))
                    (300 8 (5 10 20 40 80 100 45))))
      (destructuring-bind (timeout-ms expected-attempts expected-delays) case
        (multiple-value-bind (legacy-attempts legacy-delays)
            (legacy-lock-schedule timeout-ms)
          (expect legacy-attempts :to-be expected-attempts)
          (expect legacy-delays :to-equal expected-delays)
          (with-temp-store (plain)
            (let* ((clock-ms 0)
                   (attempts 0)
                   (delays nil)
                   (io (aitools.store.application::copy-store-io
                        (store-io-port plain)
                        :try-lock (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    (incf attempts)
                                    nil)
                        :monotonic-ms (lambda () clock-ms)
                        :sleep (lambda (milliseconds)
                                 (push milliseconds delays)
                                 (incf clock-ms milliseconds))))
                   (store (store-with-io plain io)))
              (expect
               (with-workspace-lock (store timeout-ms :on-timeout :busy)
                 :acquired)
               :to-be :busy)
              (expect attempts :to-be legacy-attempts)
              (expect (nreverse delays) :to-equal legacy-delays))))))
    (expect
     (aitools.store.application::%lock-retry-attempt-limit 1000000000)
     :to-be 10000005))
  (it "accepts acquisition on the deadline attempt"
    (with-temp-store (plain)
      (let* ((clock-ms 0)
             (attempts 0)
             (delays nil)
             (unlocked nil)
             (io (aitools.store.application::copy-store-io
                  (store-io-port plain)
                  :try-lock (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (incf attempts)
                              (when (>= clock-ms 105) :lock-handle))
                  :unlock (lambda (handle) (push handle unlocked))
                  :monotonic-ms (lambda () clock-ms)
                  :sleep (lambda (milliseconds)
                           (push milliseconds delays)
                           (incf clock-ms milliseconds))))
             (store (store-with-io plain io)))
        (expect
         (with-workspace-lock (store 105 :on-timeout :busy)
           :acquired)
         :to-be :acquired)
        (expect attempts :to-be 6)
        (expect (nreverse delays) :to-equal '(5 10 20 40 30))
        (expect unlocked :to-equal '(:lock-handle)))))
  (it "stops retrying after an overslept backoff"
    (with-temp-store (plain)
      (let* ((clock-ms 0)
             (attempts 0)
             (delays nil)
             (io (aitools.store.application::copy-store-io
                  (store-io-port plain)
                  :try-lock (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (incf attempts)
                              nil)
                  :monotonic-ms (lambda () clock-ms)
                  :sleep (lambda (milliseconds)
                           (push milliseconds delays)
                           (incf clock-ms (+ milliseconds 200)))))
             (store (store-with-io plain io)))
        (expect
         (with-workspace-lock (store 105 :on-timeout :busy)
           :acquired)
         :to-be :busy)
        (expect attempts :to-be 2)
        (expect (nreverse delays) :to-equal '(5))
        (expect clock-ms :to-be 205)))))

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
