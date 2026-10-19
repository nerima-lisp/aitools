(in-package #:aitools.integration.structure-test)

(defun %budget-relative-path (pathname root)
  (enough-namestring pathname root))

(defun %budget-hidden-path-p (pathname root)
  (some
   (lambda (part)
     (and (> (length part) 1) (char= (char part 0) #\.)))
   (uiop:split-string (%budget-relative-path pathname root) :separator "/")))

(defun %budget-pathnames (root pattern)
  (sort
   (remove-if
    (lambda (pathname)
      (%budget-hidden-path-p pathname root))
    (directory (merge-pathnames pattern root)))
   #'string<
   :key
   (lambda (pathname)
     (%budget-relative-path pathname root))))

(defun %lisp-line-count (pathname)
  (let ((text (uiop:read-file-string pathname)))
    (if (zerop (length text))
        0
        (+ (count #\Newline text)
           (if (char= (char text (1- (length text))) #\Newline)
               0
               1)))))

(defun %line-budget-baseline-table ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry *line-budget-ratchet* table)
      (setf (gethash (first entry) table) (second entry)))))

(defun %line-budget-exemption-table ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry *line-budget-generated-exemptions* table)
      (setf (gethash (first entry) table) (second entry)))))

(defun %line-budget-exemptions-valid-p ()
  (let ((paths (sort (mapcar #'first *line-budget-generated-exemptions*) #'string<)))
    (and (= (length paths) 2)
         (equal paths
                '("data/domain/text/cp932-data.lisp"
                  "data/domain/text/euc-jp-data.lisp"))
         (every
          (lambda (entry)
            (search "generate-charmap-data.pl" (second entry) :test #'char-equal))
          *line-budget-generated-exemptions*))))

(defun line-budget-violations (root)
  (let ((ratchet (%line-budget-baseline-table))
        (exemptions (%line-budget-exemption-table))
        (seen (make-hash-table :test #'equal))
        (violations '()))
    (unless (%line-budget-exemptions-valid-p)
      (push
       "line budget: generated exemptions must name only the two charmap data files with generator reasons"
       violations))
    (dolist
        (pathname
         (append (%budget-pathnames root "**/*.lisp")
                 (%budget-pathnames root "**/*.asd")))
      (let* ((relative (%budget-relative-path pathname root))
             (lines (%lisp-line-count pathname))
             (maximum (gethash relative ratchet)))
        (setf (gethash relative seen) lines)
        (unless (gethash relative exemptions)
          (cond
            ((and maximum (> lines maximum))
             (push
              (format nil
                      "~A: ~D lines exceeds its ratchet of ~D"
                      relative
                      lines
                      maximum)
              violations))
            ((and maximum (< lines maximum))
             (push
              (format nil
                      "~A: measured ~D lines is below its ratchet of ~D; lower the baseline"
                      relative
                      lines
                      maximum)
              violations))
            ((and (null maximum) (> lines *line-budget-limit*))
             (push
              (format nil
                      "~A: ~D lines exceeds the ~D-line limit without a ratchet"
                      relative
                      lines
                      *line-budget-limit*)
              violations))))))
    (dolist (entry *line-budget-ratchet*)
      (let ((lines (gethash (first entry) seen :missing)))
        (when (or (eq lines :missing) (<= lines *line-budget-limit*))
          (push
           (format nil
                   "~A: line budget ratchet is stale; current lines ~S"
                   (first entry)
                   lines)
           violations))))
    (sort violations #'string<)))

(defun %call-with-line-budget-fixture (path line-count ratchet function)
  (aitools.test.support:call-with-workspace
   (lambda (root state-directory)
     (declare (ignore state-directory))
     (let* ((*line-budget-limit* 3)
            (*line-budget-ratchet* ratchet)
            (pathname (merge-pathnames path (uiop:ensure-directory-pathname root))))
       (ensure-directories-exist pathname)
       (with-open-file (stream pathname :direction :output :if-exists :supersede)
         (loop repeat line-count
               do (write-line "(defun sample () nil)" stream)))
       (funcall function (uiop:ensure-directory-pathname root))))))

(defun %line-budget-fixture-violations (path line-count ratchet)
  (%call-with-line-budget-fixture path
                                  line-count
                                  ratchet
                                  (lambda (root)
                                    (line-budget-violations root))))

(describe "aitools structure budgets (source lines)"
          (it "keeps every Lisp file within 250 lines or its existing per-file ratchet"
              (expect *line-budget-limit* :to-equal 250)
              (expect (%line-budget-exemptions-valid-p) :to-equal t)
              (expect (line-budget-violations (repository-root)) :to-equal nil))
          (it "fails for a new unratcheted violation in a fixture"
              (expect
               (some
                (lambda (violation)
                  (search "without a ratchet" violation))
                (%line-budget-fixture-violations "src/sample.lisp" 4 nil))
               :to-be-truthy))
          (it "fails when a fixture grows above its ratchet"
              (expect
               (some
                (lambda (violation)
                  (search "exceeds its ratchet" violation))
                (%line-budget-fixture-violations "src/sample.lisp"
                                                 4
                                                 '(("src/sample.lisp" 3))))
               :to-be-truthy))
          (it "fails when a fixture is below its ratchet"
              (expect
               (some
                (lambda (violation)
                  (search "lower the baseline" violation))
                (%line-budget-fixture-violations "src/sample.lisp"
                                                 4
                                                 '(("src/sample.lisp" 5))))
               :to-be-truthy))
          (it "fails when a fixture ratchet entry is stale"
              (expect
               (some
                (lambda (violation)
                  (search "ratchet is stale" violation))
                (%line-budget-fixture-violations "src/sample.lisp"
                                                 3
                                                 '(("src/missing.lisp" 4))))
               :to-be-truthy))
          (it "accepts a clean fixture whose ratchet matches its measured value"
              (expect
               (%line-budget-fixture-violations "src/sample.lisp"
                                                4
                                                '(("src/sample.lisp" 4)))
               :to-equal
               nil)))
