(require :asdf)

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(uiop:pathname-directory-pathname (uiop:getcwd)))
   :inherit-configuration))

(defun signal-grace-probe-cache-files (directory)
  (append (uiop:directory-files directory)
          (loop for subdirectory in (uiop:subdirectories directory)
                append (signal-grace-probe-cache-files subdirectory))))

(let* ((variant (uiop:getenv "AITOOLS_SIGNAL_GRACE_VARIANT"))
       (source-revision (uiop:getenv "AITOOLS_SIGNAL_GRACE_SOURCE"))
       (trial-text (uiop:getenv "AITOOLS_SIGNAL_GRACE_TRIAL"))
       (trial (and trial-text (parse-integer trial-text :junk-allowed nil)))
       (workspace (truename (uiop:pathname-directory-pathname (uiop:getcwd))))
       (fasl-cache (merge-pathnames ".signal-grace-fasl-cache/" workspace))
       (runner-source (truename *load-truename*))
       (expected-runner (truename (merge-pathnames "t/perf/signal-grace-repetition-runner.lisp"
                                                    workspace))))
  (unless (member variant '("baseline" "fixed") :test #'string=)
    (error "AITOOLS_SIGNAL_GRACE_VARIANT must be baseline or fixed."))
  (unless (and source-revision (plusp (length source-revision)))
    (error "AITOOLS_SIGNAL_GRACE_SOURCE must identify the source revision."))
  (unless (and trial (<= 1 trial 50))
    (error "AITOOLS_SIGNAL_GRACE_TRIAL must be between 1 and 50."))
  (unless (equal runner-source expected-runner)
    (error "The repetition runner did not load from the current source checkout."))
  (asdf:clear-system "aitools")
  (asdf:load-asd (merge-pathnames "aitools.asd" workspace))
  (asdf:initialize-output-translations
   `(:output-translations
     (t (,(uiop:ensure-directory-pathname fasl-cache) :**/ :*.*.*))
     :ignore-inherited-configuration))
  (asdf:load-system "aitools/test/base" :force (= trial 1))
  (let* ((source (truename (asdf:system-source-directory "aitools")))
         (test-source (truename (asdf:system-source-directory "aitools/test/base")))
         (source-file (truename (asdf:system-source-file "aitools")))
         (test-source-file (truename (asdf:system-source-file "aitools/test/base")))
         (expected-source-file (truename (merge-pathnames "aitools.asd" workspace)))
         (expected-test-source-file
           (truename (merge-pathnames "aitools-test-system-base.lisp" workspace)))
         (fasl-files (remove-if-not (lambda (path)
                                      (equal (pathname-type path) "fasl"))
                                    (signal-grace-probe-cache-files fasl-cache))))
    (unless (and (equal workspace source) (equal workspace test-source)
                 (equal expected-source-file source-file)
                 (equal expected-test-source-file test-source-file)
                 (plusp (length fasl-files)))
      (error "The loaded systems do not belong to the current source checkout."))
    (format t "SOURCE_VARIANT=~A REVISION=~A TRIAL=~D SOURCE_ROOT=~A SOURCE_ASD=~A RUNNER_SOURCE=~A FASL_CACHE=~A FASL_COUNT=~D~%"
            variant source-revision trial (namestring source) (namestring source-file)
            (namestring runner-source) (namestring fasl-cache) (length fasl-files))))

(defun signal-grace-probe-ms (ticks)
  (float (/ (* 1000 ticks) internal-time-units-per-second) 1d0))

(defun signal-grace-probe-timestamp (observations phase)
  (let ((values (gethash phase observations)))
    (unless (and (consp values) (numberp (first values)))
      (error "Missing signal-grace probe timestamp ~S." phase))
    (first values)))

(defun signal-grace-probe-delta-ms (observations start-phase end-phase)
  (signal-grace-probe-ms
   (- (signal-grace-probe-timestamp observations end-phase)
      (signal-grace-probe-timestamp observations start-phase))))

(defun signal-grace-probe-run-once (trial variant)
  (sb-ext:gc :full t)
  (let* ((observations (make-hash-table :test #'eq))
         (observer (lambda (phase &rest values)
                     (setf (gethash phase observations) values)))
         (aitools.process.integration-test::*signal-grace-probe-observer* observer)
         (json (make-string-output-stream))
         (events (uiop:symbol-call :cl-weave :run nil
                                   :name-filter
                                   "rejects incomplete or mismatched pattern selection and sends KILL after grace"
                                   :reporter :json :stream json))
         (report (uiop:symbol-call :json-kit :parse (get-output-stream-string json)))
         (reported-events (gethash "events" report)))
    (unless (and (= (length events) 1)
                 (= (length reported-events) 1))
      (error "Trial ~D selected ~D specs and reported ~D events; expected one of each."
             trial (length events) (length reported-events)))
    (let* ((event (elt reported-events 0))
           (status (gethash "status" event))
           (window (gethash :test-window observations)))
      (unless (member status '("pass" "fail") :test #'string=)
        (error "Trial ~D ended with non-assertion status ~S." trial status))
      (unless (and (consp window) (= (length window) 2)
                   (every #'numberp window))
        (error "Trial ~D did not record exactly one test timing window." trial))
      (let* ((started (first window))
             (ended (second window))
             (elapsed (- ended started))
             (below-300ms (< elapsed (* 0.3 internal-time-units-per-second)))
             (failed (string= status "fail")))
        (dolist (phase '(:invoke-start :registry-created :ports-registered
                         :commands-finalized :app-created :dispatch-start
                         :term-completed :wait-first-clock-return :kill-completed
                         :dispatch-returned :output-extracted :json-parsed))
          (signal-grace-probe-timestamp observations phase))
        (format t "TRIAL=~2,'0D VARIANT=~A ELAPSED_MS=~,3F ASSERTION=~A BELOW_300MS=~A~%"
                trial variant (signal-grace-probe-ms elapsed)
                (string-upcase status) (if below-300ms "yes" "no"))
        (format t "  INSTRUMENTED_MS test_start_to_TERM=~,3F invoke_setup=~,3F registry=~,3F ports_registration=~,3F commands_finalize=~,3F app_create=~,3F dispatch_to_TERM=~,3F TERM_to_first_production_clock_return=~,3F wait_clock_to_KILL=~,3F TERM_to_KILL=~,3F KILL_to_dispatch_return=~,3F output_extract=~,3F JSON_parse=~,3F post_JSON_to_test_end=~,3F KILL_to_test_end=~,3F~%"
                (signal-grace-probe-ms (- (signal-grace-probe-timestamp observations :term-completed)
                                          started))
                (signal-grace-probe-delta-ms observations :invoke-start :dispatch-start)
                (signal-grace-probe-delta-ms observations :invoke-start :registry-created)
                (signal-grace-probe-delta-ms observations :registry-created :ports-registered)
                (signal-grace-probe-delta-ms observations :ports-registered :commands-finalized)
                (signal-grace-probe-delta-ms observations :commands-finalized :app-created)
                (signal-grace-probe-delta-ms observations :dispatch-start :term-completed)
                (signal-grace-probe-delta-ms observations :term-completed :wait-first-clock-return)
                (signal-grace-probe-delta-ms observations :wait-first-clock-return :kill-completed)
                (signal-grace-probe-delta-ms observations :term-completed :kill-completed)
                (signal-grace-probe-delta-ms observations :kill-completed :dispatch-returned)
                (signal-grace-probe-delta-ms observations :dispatch-returned :output-extracted)
                (signal-grace-probe-delta-ms observations :output-extracted :json-parsed)
                (signal-grace-probe-ms (- ended (signal-grace-probe-timestamp observations :json-parsed)))
                (signal-grace-probe-ms (- ended (signal-grace-probe-timestamp observations :kill-completed))))
        (format t "TRIAL_SUMMARY VARIANT=~A SELECTED=1 PASSED=~D BELOW_300MS=~D FAILED_ASSERTIONS=~D~%"
                variant (if failed 0 1) (if below-300ms 1 0) (if failed 1 0))
        (values below-300ms failed)))))

(let* ((variant (uiop:getenv "AITOOLS_SIGNAL_GRACE_VARIANT"))
       (trial (parse-integer (uiop:getenv "AITOOLS_SIGNAL_GRACE_TRIAL") :junk-allowed nil)))
  (signal-grace-probe-run-once trial variant))

(uiop:quit 0)
