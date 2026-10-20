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

  (defun read-coverage-baseline (root context)
    (let ((path
            (if (eq context :aggregate)
                (merge-pathnames "coverage-baseline.lisp" root)
                (merge-pathnames
                 (format nil "coverage-baselines/~(~A~).lisp" context) root))))
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
          (when (nth-value 0
                           (get-properties
                            baseline
                            (list :uncovered-in-package-expressions)))
            (let ((count (getf baseline :uncovered-in-package-expressions)))
              (unless (and (integerp count) (not (minusp count)))
                (error "Invalid uncovered IN-PACKAGE expression count for ~S: ~S"
                       context count))))
          baseline))))

  (defun uncovered-in-package-expression-p (source location)
    (and (eq (getf location :kind) :expression)
         (let* ((*read-eval* nil)
                (*package* (find-package "CL-USER"))
                (form (read-from-string source nil nil
                                        :start (getf location :start)))
                (operator (and (consp form) (car form))))
           (eq operator
               (find-symbol "IN-PACKAGE" "COMMON-LISP")))))

  (defun uncovered-in-package-expression-count (file-statistics)
    (loop for file in file-statistics
          for source = (uiop:read-file-string (getf file :pathname))
          sum (count-if (lambda (location)
                          (uncovered-in-package-expression-p source location))
                        (getf file :uncovered-forms))))

  (defun adjust-context-expression-total (statistics context current-count baseline)
    (multiple-value-bind (indicator baseline-count)
        (get-properties baseline (list :uncovered-in-package-expressions))
      (cond
        ((null indicator) statistics)
        (t
         (unless (and (integerp current-count) (not (minusp current-count))
                      (integerp baseline-count) (not (minusp baseline-count)))
           (error "Invalid uncovered IN-PACKAGE expression counts for ~S: ~S/~S"
                  context current-count baseline-count))
         (multiple-value-bind (covered total)
             (require-positive-coverage-counts statistics context :expression)
           (let ((adjusted-total (- total (- current-count baseline-count))))
             (unless (and (integerp adjusted-total)
                          (plusp adjusted-total)
                          (<= covered adjusted-total))
               (error "Invalid adjusted ~A expression coverage total: ~D"
                      context adjusted-total))
             (let ((adjusted (copy-list statistics)))
               (setf (getf adjusted :expression-total) adjusted-total
                     (getf adjusted :uncovered-in-package-expressions) current-count
                     (getf adjusted :baseline-uncovered-in-package-expressions)
                     baseline-count)
               adjusted))))))))

