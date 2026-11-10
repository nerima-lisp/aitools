(in-package #:cl-user)

(defparameter +coverage-gap-report-limit+ 200)

(defun coverage-gap-relative-path (root pathname)
  (let* ((root-name (namestring (uiop:ensure-directory-pathname (truename root))))
         (source-name (namestring (truename pathname))))
    (unless (and (<= (length root-name) (length source-name))
                 (string= root-name source-name :end2 (length root-name)))
      (error "Coverage source is outside the repository root: ~A"
             (file-namestring pathname)))
    (subseq source-name (length root-name))))

(defun coverage-gap-line-number (location source)
  (let ((start (getf location :start)))
    (unless (and (integerp start) (<= 0 start (length source)))
      (error "Invalid coverage location in source file"))
    (1+ (count #\Newline source :end start))))

(defun coverage-gap-location-excluded-p (location owner exclusions)
  (some (lambda (exclusion)
          (and owner
               (eq (getf exclusion :kind) (getf location :kind))
               (string-equal (getf exclusion :package) (getf owner :package))
               (string-equal (getf exclusion :symbol) (getf owner :symbol))))
        exclusions))

(defun coverage-gap-records (root context locations exclusions)
  (let* ((pathnames
           (remove-duplicates
            (remove nil (mapcar (lambda (location) (getf location :pathname)) locations))
            :test (function equal)))
         (definitions
           (loop for pathname in pathnames
                 append (coverage-source-definitions-for pathname)))
         (sources (make-hash-table :test (function equal))))
    (dolist (pathname pathnames)
      (setf (gethash pathname sources) (uiop:read-file-string pathname)))
    (loop for location in locations
          for owner = (coverage-owner-for-location location definitions)
          unless (coverage-gap-location-excluded-p location owner exclusions)
            collect
            (list :context context
                  :kind (string-upcase (symbol-name (getf location :kind)))
                  :owner (if owner
                             (format nil "~A::~A"
                                     (getf owner :package) (getf owner :symbol))
                             "unknown-owner")
                  :path (coverage-gap-relative-path root (getf location :pathname))
                  :line (coverage-gap-line-number
                         location (gethash (getf location :pathname) sources))))))

(defun report-coverage-gap-records (context records &optional (stream *standard-output*))
  (let ((reported 0)
        (omitted 0))
    (dolist (record records)
      (if (< reported +coverage-gap-report-limit+)
          (progn
            (format stream "coverage gap ~A ~A ~A ~A:~D~%"
                    context (getf record :kind) (getf record :owner)
                    (getf record :path) (getf record :line))
            (incf reported))
          (incf omitted)))
    (when (plusp omitted)
      (format stream "coverage gap ~A: ~D additional uncovered site~:P omitted~%"
              context omitted))))

(defun report-coverage-gaps (root groups measured)
  (dolist (measurement measured)
    (let* ((context (first measurement))
           (group (assoc context groups))
           (locations (coverage-uncovered-form-locations-for (cdr group)))
           (records
             (coverage-gap-records root context locations (fifth measurement))))
      (report-coverage-gap-records context records))))
