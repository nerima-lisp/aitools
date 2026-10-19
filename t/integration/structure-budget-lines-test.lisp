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
  (let ((paths
         (sort (mapcar #'first *line-budget-generated-exemptions*) #'string<)))
    (and (= (length paths) 2)
         (equal paths
                '("data/domain/text/cp932-data.lisp"
                  "data/domain/text/euc-jp-data.lisp"))
         (every
          (lambda (entry)
            (search "generate-charmap-data.pl"
                    (second entry)
                    :test
                    #'char-equal))
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
    (dolist (pathname (append
                             (%budget-pathnames root "**/*.lisp")
                             (%budget-pathnames root "**/*.asd")))
      (let* ((relative (%budget-relative-path pathname root))
             (lines (%lisp-line-count pathname))
             (maximum (gethash relative ratchet)))
        (setf (gethash relative seen) lines)
        (unless (gethash relative exemptions)
          (cond
            ((and maximum (> lines maximum))
             (push
              (format nil "~A: ~D lines exceeds its ratchet of ~D"
                      relative lines maximum)
              violations))
            ((and (null maximum) (> lines *line-budget-limit*))
             (push
              (format nil
                      "~A: ~D lines exceeds the ~D-line limit without a ratchet"
                      relative lines *line-budget-limit*)
              violations))))))
    (dolist (entry *line-budget-ratchet*)
      (let ((lines (gethash (first entry) seen :missing)))
        (when (or (eq lines :missing) (<= lines *line-budget-limit*))
          (push
           (format nil "~A: line budget ratchet is stale; current lines ~S"
                   (first entry) lines)
           violations))))
    (sort violations #'string<)))

(describe "aitools structure budgets (source lines)"
          (it
           "keeps every Lisp file within 250 lines or its existing per-file ratchet"
           (expect *line-budget-limit* :to-equal 250)
           (expect (%line-budget-exemptions-valid-p) :to-equal t)
           (expect (line-budget-violations (repository-root)) :to-equal nil)))
