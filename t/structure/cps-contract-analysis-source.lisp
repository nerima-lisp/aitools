(in-package #:aitools.integration.cps-contract-test)

(defun %source-violations (text relative-path)
  (loop with package-name = nil for top-level in (%source-forms text)
        do (when (%operator-name-p top-level "IN-PACKAGE")
             (setf package-name (string-upcase (string (second top-level)))))
        append (loop for form in (%top-level-definitions top-level) append
                     (cond
                       ((%operator-name-p form "DEFUN")
                        (%definition-violations form relative-path package-name))
                       ((%operator-name-p form "DEFINE-WRITE-COMMAND")
                        (let* ((command-name (second form))
                               (name (and (stringp command-name)
                                          (format nil
                                                  "PREPARE-~A/K"
                                                  (substitute #\- #\. (string-upcase command-name)))))
                               (variables (%continuation-variables (third form))))
                          (when name
                            (mapcar
                             (lambda (kind) (list relative-path name kind))
                             (%flow-body-kinds (cdddr form) variables)))))
                       (t (%flow-violations form relative-path))))))

(defun %source-pathnames (root)
  (sort (remove-duplicates
         (append (directory (merge-pathnames "packages/**/src/**/*.lisp" root))
                 (directory (merge-pathnames "src/**/*.lisp" root)))
         :test #'equal)
        #'string< :key #'namestring))

(defun %key-order (entry)
  (format nil "~A|~A|~A" (first entry) (second entry) (third entry)))

(defun %cps-contract-violations (root)
  (let ((violations '()))
    (dolist (pathname (%source-pathnames root))
      (setf violations
            (nconc violations
                   (%source-violations (uiop:read-file-string pathname)
                                       (enough-namestring pathname root)))))
    (sort (remove-duplicates violations :test #'equal)
          #'string< :key #'%key-order)))
