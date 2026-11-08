(in-package #:cl-user)
(progn
  (defun coverage-counts (statistics metric)
    (let ((keys (ecase metric
                  (:expression '(:expression-covered :expression-total))
                  (:branch '(:branch-covered :branch-total)))))
      (values (getf statistics (first keys)) (getf statistics (second keys)))))

  (defun require-positive-coverage-counts (statistics context metric)
    (multiple-value-bind (covered total) (coverage-counts statistics metric)
      (unless (and (integerp covered) (integerp total) (plusp covered)
                   (plusp total) (<= covered total))
        (error "~A ~A coverage counts are missing, zero, or invalid: ~S/~S"
               context metric covered total))
      (values covered total)))

  (progn
    (defun coverage-baseline-path (root context)
      (if (eq context :aggregate)
          (merge-pathnames "coverage-baseline.lisp" root)
          (merge-pathnames (format nil "coverage-baselines/~(~A~).lisp" context) root)))
    (defun read-coverage-baseline (root context)
      (let ((path (coverage-baseline-path root context)))
        (unless (probe-file path)
          (error "Missing coverage baseline for ~S: ~A" context path))
        (with-open-file (stream path)
          (let* ((*read-eval* nil) (baseline (read stream nil :eof)))
            (unless (and (listp baseline) (evenp (length baseline))
                         (eq (getf baseline :context) context)
                         (eq (getf baseline :state) :measured)
                         (eq (getf baseline :source) :linux-ci))
              (error "Invalid coverage baseline for ~S: ~A" context path))
            (unless (eq (read stream nil :eof) :eof)
              (error "Coverage baseline has trailing data: ~A" path))
            (dolist (metric '(:expression :branch))
              (require-positive-coverage-counts baseline context metric))
            baseline))))
    (defun read-coverage-baseline-if-present (root context)
      (when (probe-file (coverage-baseline-path root context))
        (read-coverage-baseline root context)))
    (progn
      (defun require-context-coverage-baselines (measured)
        (let ((missing (loop for entry in measured
                             unless (fourth entry) collect (first entry))))
          (when missing
            (error "Missing coverage baselines for contexts: ~{~A~^, ~}" missing)))
        t)
      (defun coverage-measurement-record (context source-count statistics baseline)
        (list :context context :source-count source-count
              :baseline-state (getf baseline :state) :baseline-source (getf baseline :source)
              :expression-covered (getf statistics :expression-covered)
              :expression-total (getf statistics :expression-total)
              :branch-covered (getf statistics :branch-covered)
              :branch-total (getf statistics :branch-total)))
      (defun write-context-measurements (root aggregate contexts exclusions)
        (let* ((path (merge-pathnames "coverage/context-measurements.sexp" root))
               (record (list :schema-version 1 :platform (software-type) :aggregate aggregate
                             :contexts contexts :exclusions exclusions)))
          (ensure-directories-exist path)
          (with-open-file (stream path :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
            (write record :stream stream :pretty t :readably t) (terpri stream))
          (format t "~&coverage-context-measurements-sexp ")
          (write record :stream *standard-output* :readably t)
          (terpri)
          path)))))

