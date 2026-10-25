(require :asdf)

(progn
  (defparameter *baseline-revision* "1b20c3abdea1d95a049cdb0481afcfd72072b2a4")
  (defparameter *benchmark-mode* :compare)
  (defparameter *self-arm* :old)
  (defparameter *signature-hash-function* nil))
(defparameter *warmup-pairs* 6)
(defparameter *sample-pairs* 80)

(defun %project-root ()
  (let ((root (uiop:ensure-directory-pathname (uiop:getcwd))))
    (unless (probe-file (merge-pathnames "aitools.asd" root))
      (error "Run this benchmark from the aitools project root."))
    root))
(defun %temporary-directory (root)
  (let ((directory
          (merge-pathnames
           (format nil ".digest-deflate-adapter-benchmark-~36R-~36R/"
                   (get-universal-time) (random (expt 36 10)))
           root)))
    (when (probe-file directory)
      (error "Benchmark temporary directory already exists: ~A" directory))
    (ensure-directories-exist (merge-pathnames "source.lisp" directory))
    directory))

(defun %extract-baseline-source (root directory relative-path name)
  (let* ((source (merge-pathnames (format nil "~A.lisp" name) directory))
         (fasl (merge-pathnames (format nil "~A.fasl" name) directory))
         (text (uiop:run-program
                (list "jj" "file" "show" "--ignore-working-copy"
                      "-r" *baseline-revision* relative-path)
                :directory root :output :string :error-output :string)))
    (unless (plusp (length text))
      (error "Historical source is empty: ~A" relative-path))
    (with-open-file (stream source :direction :output :if-exists :error
                                    :if-does-not-exist :create)
      (write-string text stream))
    (multiple-value-bind (compiled warnings-p failure-p)
        (compile-file source :output-file fasl)
      (declare (ignore warnings-p))
      (when failure-p
        (error "Could not compile baseline source ~A" relative-path))
      (load compiled))))

(defun %make-input (size seed)
  (let ((input (make-array size :element-type '(unsigned-byte 8)))
        (state seed))
    (loop for index below size
          do (setf state (logand #xffffffff (logxor state (ash state 13)))
                   state (logxor state (ash state -17))
                   state (logand #xffffffff (logxor state (ash state 5)))
                   (aref input index) (ldb (byte 8 0) state)))
    input))

(defun %timed-invoke (function)
  (sb-ext:gc :full t)
  (let ((start (get-internal-real-time)))
    (let ((values (multiple-value-list (funcall function))))
      (values (* 1000.0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))
              values))))

(defun %paired-invoke (first-function second-function first-first-p)
  (if first-first-p
      (multiple-value-bind (first-time first-values)
          (%timed-invoke first-function)
        (multiple-value-bind (second-time second-values)
            (%timed-invoke second-function)
          (values first-time first-values second-time second-values)))
      (multiple-value-bind (second-time second-values)
          (%timed-invoke second-function)
        (multiple-value-bind (first-time first-values)
            (%timed-invoke first-function)
          (values first-time first-values second-time second-values)))))
(defun %median (values)
  (let* ((ordered (sort (copy-list values) #'<))
         (count (length ordered))
         (middle (floor count 2)))
    (if (oddp count)
        (nth middle ordered)
        (/ (+ (nth (1- middle) ordered) (nth middle ordered)) 2.0))))
(defun %relative-change-percent (first-time second-time)
  (unless (plusp first-time)
    (error "First-arm elapsed time is zero; cannot compute a paired difference."))
  (* 100.0 (/ (- second-time first-time) first-time)))

(defun %run-workload (name input-size old-function current-function validator)
  (let* ((compare-p (eq *benchmark-mode* :compare))
         (first-label
           (if compare-p "old"
               (string-downcase (symbol-name *self-arm*))))
         (second-label (if compare-p "current" first-label))
         (first-function
           (if (or compare-p (eq *self-arm* :old))
               old-function current-function))
         (second-function (if compare-p current-function first-function)))
    (format t "~&Workload: ~A; input=~:D bytes; arms=~A/~A; warmup pairs=~D; sample pairs=~D; order alternates; full GC before each call.~%"
            name input-size first-label second-label
            *warmup-pairs* *sample-pairs*)
    (format t "# raw-timing-v1~%row_type~Cworkload~Cpair~Carm_a~Carm_b~Corder~Ctime_a_ms~Ctime_b_ms~Crelative_difference_percent~Ccorrectness~Coutput_signature_a~Coutput_signature_b~%"
            #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab
            #\Tab #\Tab #\Tab #\Tab #\Tab)
    (dotimes (index *warmup-pairs*)
      (multiple-value-bind (first-time first-values second-time second-values)
          (%paired-invoke first-function second-function (evenp index))
        (declare (ignore first-time second-time))
        (funcall validator first-values second-values)))
    (let ((first-times nil)
          (second-times nil)
          (relative-changes nil)
          (second-wins 0))
      (dotimes (index *sample-pairs*)
        (multiple-value-bind (first-time first-values second-time second-values)
            (%paired-invoke first-function second-function
                            (evenp (+ *warmup-pairs* index)))
          (funcall validator first-values second-values)
          (let ((relative (%relative-change-percent first-time second-time))
                (first-signature (%output-signature first-values))
                (second-signature (%output-signature second-values)))
            (push first-time first-times)
            (push second-time second-times)
            (push relative relative-changes)
            (when (< second-time first-time)
              (incf second-wins))
            (format t "sample~C~A~C~D~C~A~C~A~C~A~C~,6F~C~,6F~C~,6F~Cpass~C~A~C~A~%"
                    #\Tab name #\Tab (1+ index) #\Tab
                    first-label #\Tab second-label #\Tab
                    (if (evenp (+ *warmup-pairs* index))
                        "arm-a/arm-b" "arm-b/arm-a")
                    #\Tab first-time #\Tab second-time #\Tab relative
                    #\Tab #\Tab first-signature #\Tab second-signature))))
      (setf first-times (nreverse first-times)
            second-times (nreverse second-times)
            relative-changes (nreverse relative-changes))
      (format t "  median arm times: ~A=~,6F ms, ~A=~,6F ms; median relative difference=~,6F% (positive means arm B slower); arm B (~A) faster in ~D/~D pairs.~%"
              first-label (%median first-times)
              second-label (%median second-times)
              (%median relative-changes)
              second-label second-wins *sample-pairs*)
      (multiple-value-bind (lower upper lower-rank upper-rank)
          (%median-interval relative-changes)
        (if lower-rank
            (format t "  95%+ distribution-free median interval: [~,6F%, ~,6F%] (order statistics ~D and ~D).~%"
                    lower upper lower-rank upper-rank)
            (format t "  95%+ distribution-free median interval unavailable for ~D pairs.~%"
                    *sample-pairs*))))))

(defun %assert-sha-parity (old-values current-values expected)
  (let ((old-hash (first old-values))
        (current-hash (first current-values)))
    (unless (and (stringp old-hash) (stringp current-hash) (string= old-hash current-hash)
                 (string= current-hash expected))
      (error "SHA-256 outputs differ or failed the reference check."))))
(defun %assert-inflate-result (values expected encoded)
  (let ((output (first values))
        (consumed (second values)))
    (unless (and (equalp output expected) (eql consumed (length encoded)))
      (error "Inflate output or consumed length is incorrect."))))
(defun %assert-deflate-parity (old-values current-values input old-inflate current-inflate)
  (dolist (encoded (list (first old-values) (first current-values)))
    (unless (typep encoded '(simple-array (unsigned-byte 8) (*)))
      (error "Deflate did not return an octet vector."))
    (%assert-inflate-result (multiple-value-list (funcall old-inflate encoded)) input encoded)
    (%assert-inflate-result (multiple-value-list (funcall current-inflate encoded))
                            input encoded)))
(defun %assert-inflate-parity (old-values current-values input encoded)
  (%assert-inflate-result old-values input encoded)
  (%assert-inflate-result current-values input encoded)
  (unless (equalp (first old-values) (first current-values)) (error "Old and current inflate outputs differ.")))
  (progn
  (load (merge-pathnames
         "digest-deflate-adapter-benchmark-runner.lisp"
         *load-truename*))
  (%parse-command-line-options))
  (run-digest-deflate-adapter-benchmark)
