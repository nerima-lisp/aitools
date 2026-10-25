(in-package #:aitools/test)

(defun %read-coverage-sidecar-summary (pathname expected-paths)
  ;; CL-Weave writes SB-COVER's readable object despite the .json suffix.
  (with-open-file (stream pathname)
    (let* ((*read-eval* nil)
           (entries (read stream nil :eof)))
      (when (eq entries :eof)
        (error "Coverage sidecar is empty: ~A" pathname))
      (unless (and (listp entries) entries)
        (error "Coverage sidecar has no file entries: ~A" pathname))
      (unless (eq (read stream nil :eof) :eof)
        (error "Coverage sidecar has trailing data: ~A" pathname))
      (let* ((expected-keys
              (mapcar (lambda (path)
                        (if (pathnamep path) (namestring path) path))
                      expected-paths))
             (included (make-hash-table :test (function equal)))
             (seen (make-hash-table :test (function equal)))
             (expression-count 0))
        (unless (and expected-keys (every (function stringp) expected-keys))
          (error "Coverage input has no file paths: ~A" pathname))
        (dolist (path expected-keys)
          (when (gethash path included)
            (error "Duplicate coverage input path: ~A" path))
          (setf (gethash path included) t))
        (dolist (entry entries)
          (unless (and (consp entry)
                       (stringp (first entry))
                       (vectorp (second entry))
                       (bit-vector-p (cddr entry))
                       (= (length (second entry)) (length (cddr entry))))
            (error "Malformed coverage sidecar file entry: ~S" entry))
          (let ((path (first entry)))
            (when (gethash path included)
              (when (gethash path seen)
                (error "Coverage sidecar has duplicate included file: ~A" path))
              (setf (gethash path seen) t)
              (incf expression-count
                    ;; SB-COVER's path vector includes branch arms; HTML counts them separately.
                    (count-if (lambda (path)
                                (not (or (eq (first path) :then) (eq (first path) :else))))
                              (second entry))))))
        (dolist (path expected-keys)
          (unless (gethash path seen)
            (error "Coverage sidecar is missing included file: ~A" path)))
        (unless (plusp expression-count)
          (error "Coverage sidecar has no expression counts: ~A" pathname))
        (values (hash-table-count seen) expression-count)))))

(defun %require-cover-index-markup (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (unless (plusp (file-length stream))
      (error "Coverage HTML report is empty: ~A" pathname)))
  (let ((html (uiop:read-file-string pathname)))
    (dolist
        (expected
         '("<table class='summary'>" "Source file" "Expression" "Branch"))
      (unless (search expected html)
        (error "Coverage HTML report is missing expected markup ~S: ~A"
               expected
               pathname)))
    (%coverage-html-summary-counts html pathname)))

(defun %coverage-html-cell-values (row pathname)
  (loop with position = 0
        for cell-start = (search "<td" row :start2 position)
        while cell-start
        for content-start = (position #\> row :start cell-start)
        for content-end = (and content-start
                               (search "</td>" row :start2 (1+ content-start)))
        do (unless content-end
             (error "Malformed coverage HTML table row: ~A" pathname))
        collect (subseq row (1+ content-start) content-end)
        do (setf position (+ content-end (length "</td>")))))

(defun %coverage-html-expression-total (row pathname)
  (let ((cells (%coverage-html-cell-values row pathname)))
    (unless (and (>= (length cells) 3) (search "class='text-cell'" row))
      (error "Malformed coverage HTML source row: ~A" pathname))
    (let ((total
           (handler-case (parse-integer (third cells))
             (error ()
               (error "Malformed coverage HTML expression total: ~A" pathname)))))
      (when (minusp total)
        (error "Malformed coverage HTML expression total: ~A" pathname))
      total)))

(defun %coverage-html-summary-counts (html pathname)
  (let* ((table-start (search "<table class='summary'>" html))
         (table-end
          (and table-start (search "</table>" html :start2 table-start)))
         (position table-start)
         (directory "")
         (file-count 0)
         (expression-count 0)
         (source-paths '()))
    (unless (and table-start table-end)
      (error "Coverage HTML report is missing its summary table: ~A" pathname))
    (loop for row-start = (search "<tr" html :start2 position :end2 table-end)
          while row-start
          for row-end = (search "</tr>" html :start2 row-start :end2 table-end)
          do (progn
               (unless row-end
                 (error "Malformed coverage HTML source row: ~A" pathname))
               (let* ((row (subseq html row-start row-end))
                      (cells (%coverage-html-cell-values row pathname)))
                 (cond
                   ((search "class='subheading'" row)
                    (setf directory (or (first cells) "")))
                   ((or (search "class='odd'" row)
                        (search "class='even'" row))
                    (let* ((source-cell (first cells))
                           (link-start (and source-cell (search "<a " source-cell)))
                           (content-start (and link-start
                                               (position #\> source-cell :start link-start)))
                           (content-end (and content-start
                                             (search "</a>" source-cell
                                                     :start2 (1+ content-start)))))
                      (unless (and content-start content-end)
                        (error "Malformed coverage HTML source link: ~A" pathname))
                      (incf file-count)
                      (incf expression-count
                            (%coverage-html-expression-total row pathname))
                      (let ((source-name
                              (subseq source-cell
                                      (1+ content-start)
                                      content-end)))
                        (push (if (and (<= (length directory) (length source-name))
                                       (string= directory
                                                source-name
                                                :end2 (length directory)))
                                  source-name
                                  (concatenate 'string directory source-name))
                              source-paths))))))
               (setf position (+ row-end (length "</tr>"))))
          finally (return (values file-count
                                  expression-count
                                  (nreverse source-paths))))))

(defun %validate-coverage-artifacts
    (coverage-path html-path &optional (expected-paths nil expected-paths-p))
  (multiple-value-bind (html-file-count html-expression-count html-paths)
      (%require-cover-index-markup html-path)
    (multiple-value-bind (file-count expression-count)
        (%read-coverage-sidecar-summary coverage-path
                                        (if expected-paths-p
                                            expected-paths
                                            html-paths))
      (unless (plusp file-count)
        (error "Coverage sidecar has no files: ~A" coverage-path))
      (unless (= file-count html-file-count)
        (error
         "Coverage HTML file count mismatch: sidecar has ~D, report has ~D: ~A"
         file-count
         html-file-count
         html-path))
      (unless (= expression-count html-expression-count)
        (error
         "Coverage HTML expression count mismatch: sidecar has ~D, report has ~D: ~A"
         expression-count
         html-expression-count
         html-path))
      (when expected-paths-p
        (let* ((root (uiop:pathname-directory-pathname coverage-path))
               (expected-keys
                 (mapcar (lambda (path) (namestring (merge-pathnames path root)))
                         expected-paths))
               (html-keys
                 (mapcar (lambda (path) (namestring (merge-pathnames path root)))
                         html-paths))
               (missing (set-difference expected-keys html-keys :test (function equal)))
               (unexpected (set-difference html-keys expected-keys :test (function equal))))
          (when (or missing unexpected)
            (error "Coverage HTML file paths mismatch: missing ~S, unexpected ~S: ~A"
                   missing
                   unexpected
                   html-path))))
      (values file-count expression-count))))
