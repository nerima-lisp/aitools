(in-package #:aitools.integration.cps-contract-test)

(defun %source-definition-descriptor (form relative-path)
  (cond
    ((%operator-name-p form "DEFUN")
     (let ((symbol (second form)))
       (list :path relative-path
             :kind :defun
             :symbol (%function-name-string symbol)
             :function-symbol (and (symbolp symbol) symbol)
             :lambda-list (third form)
             :body (cdddr form))))
    ((%operator-name-p form "DEFINE-FLOW")
     (let* ((header (second form))
            (symbol (first header)))
       (list :path relative-path
             :kind :define-flow
             :symbol (%function-name-string symbol)
             :function-symbol (and (symbolp symbol) symbol)
             :lambda-list (rest header)
             :body (cddr form))))
    ((%operator-name-p form "DEFINE-WRITE-COMMAND")
     (let ((command-name (second form)))
       (list :path relative-path
             :kind :define-write-command
             :symbol (and (stringp command-name)
                          (format nil "PREPARE-~A/K"
                                  (substitute #\- #\. (string-upcase command-name))))
             :function-symbol nil
             :lambda-list (third form)
             :body (cdddr form))))))

(defun %source-definitions (text relative-path)
  (loop for top-level in (%source-forms text)
        append (loop for form in (%top-level-definitions top-level)
                     for definition = (%source-definition-descriptor
                                       form relative-path)
                     when definition collect definition)))

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
  (let ((violations '())
        (definitions '()))
    (dolist (pathname (%source-pathnames root))
      (let* ((text (uiop:read-file-string pathname))
             (relative-path (enough-namestring pathname root)))
        (setf violations
              (nconc violations (%source-violations text relative-path))
              definitions
              (nconc definitions (%source-definitions text relative-path)))))
    (setf violations
          (nconc violations
                 (mapcar (lambda (record)
                           (list (getf record :definition-path)
                                 (getf record :symbol)
                                 :positional-continuation-defun))
                         (%positional-helper-inventory definitions))))
    (sort (remove-duplicates violations :test #'equal)
          #'string< :key #'%key-order)))
