(in-package #:aitools.integration.structure-test)

(defun %liveness-form-find-symbols (form record packages)
  "FIND-SYMBOL calls resolved to the Common Lisp function outside quoted data."
  (let ((calls nil))
    (labels ((name-of (object)
               (and (symbolp object)
                    (not (keywordp object))
                    (%liveness-symbol-name object)))
             (common-lisp-package-p (package)
               (member package '("CL" "COMMON-LISP") :test #'string-equal))
             (source-unqualified-p (object)
               (or (get object :u05-source-name)
                   (eq (symbol-package object) *reader-package*)))
             (cl-find-symbol-p (object)
               (let ((origin (and (symbolp object)
                                  (get object :u05-qualified-reference))))
                 (and (equal (name-of object) "FIND-SYMBOL")
                      (or (and origin
                               (common-lisp-package-p (first origin))
                               (equal (second origin) "FIND-SYMBOL"))
                          (and (symbolp object)
                               (source-unqualified-p object)
                               (let* ((own (getf record :package))
                                      (info (and own (gethash own packages)))
                                      (imports (getf info :imports))
                                      (import (find "FIND-SYMBOL" imports
                                                    :key #'second :test #'string=)))
                                 (and info
                                      (or (and import
                                               (common-lisp-package-p (first import)))
                                          (and (null import)
                                               (not (member "FIND-SYMBOL"
                                                            (getf info :shadows)
                                                            :test #'string=))
                                               (or (member "CL" (getf info :uses)
                                                           :key #'first :test #'string-equal)
                                                   (member "COMMON-LISP" (getf info :uses)
                                                           :key #'first :test #'string-equal)
                                                   (not (getf info :uses-declared-p))))))))))))
             (lexically-bound-p (object functions)
               (some (lambda (binding)
                       (and (cl-find-symbol-p binding)
                            (equal (name-of binding) (name-of object))))
                     functions))
             (walk-sequence (forms functions)
               (dolist (form forms) (walk form functions)))
             (walk-quasiquote-template (template depth functions)
               (cond
                 ((not (consp template)) nil)
                 ((eq (first template) :u05-quasiquote)
                  (walk-quasiquote-template (second template) (1+ depth) functions))
                 ((member (first template) '(:u05-unquote :u05-unquote-splicing))
                  (if (= depth 1)
                      (walk (second template) functions)
                      (walk-quasiquote-template (second template) (1- depth) functions)))
                 (t
                  (walk-quasiquote-template (car template) depth functions)
                  (walk-quasiquote-template (cdr template) depth functions))))
             (walk (object functions)
               (when (consp object)
                 (let ((head (first object))
                       (operator (name-of (first object))))
                   (cond
                     ((eq head :u05-quasiquote)
                      (walk-quasiquote-template (second object) 1 functions))
                     ((member head '(:u05-unquote :u05-unquote-splicing))
                      (walk (second object) functions))
                     ((equal operator "QUOTE") nil)
                     (t
                      (when (and (cl-find-symbol-p head)
                                 (not (lexically-bound-p head functions)))
                        (push (list (get head :u05-source-position) object) calls))
                      (if (member operator '("FLET" "LABELS") :test #'equal)
                          (let* ((definitions (second object))
                                 (names (mapcar #'first definitions))
                                 (local-functions
                                   (remove-if-not #'cl-find-symbol-p names))
                                 (definition-functions
                                   (if (equal operator "LABELS")
                                       (append local-functions functions)
                                       functions)))
                            (dolist (definition definitions)
                              (walk-sequence (cddr definition) definition-functions))
                            (walk-sequence (cddr object)
                                           (append local-functions functions)))
                          (progn
                            (walk (car object) functions)
                            (walk (cdr object) functions)))))))))
      (walk form nil)
      (nreverse calls))))

(defparameter *handwritten-stub-allowances* nil)

(defun %handwritten-production-package-p (name)
  (let ((normalized (string-upcase name)))
    (or (member normalized '("AITOOLS.DATA" "AITOOLS/CLI") :test #'string=)
        (loop for context in *all-contexts*
              thereis (loop for layer in '(:domain :application :infrastructure :presentation)
                            thereis (string= normalized (context-package-name context layer)))))))

(defun %handwritten-unique-candidates (candidates)
  (let ((seen (make-hash-table :test #'equal))
        (unique nil))
    (dolist (candidate candidates)
      (let ((key (if (eq (first candidate) :dynamic-fdefinition)
                     (subseq candidate 0 3)
                     (subseq candidate 0 2))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push candidate unique))))
    (nreverse unique)))

(defun %handwritten-allowance (key allowances)
  (find-if (lambda (entry)
             (and (consp entry) (consp (rest entry))
                  (stringp (first entry)) (stringp (second entry))
                  (equal key (list (string-upcase (first entry))
                                   (string-upcase (second entry))))))
           allowances))

(defun %handwritten-allowance-reason-p (entry &optional (index 2))
  (let ((reason (nth index entry)))
    (and (stringp reason)
         (not (string= ""
                       (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    reason))))))

(defun %handwritten-results (candidates unresolved allowances)
  (let ((violations nil)
        (used (make-hash-table :test #'eq)))
    (dolist (candidate (%handwritten-unique-candidates candidates))
      (if (eq (first candidate) :dynamic-fdefinition)
          (let* ((key (subseq candidate 0 3))
                 (allowance
                   (find-if (lambda (entry)
                              (and (consp entry)
                                   (eq (first entry) :dynamic-fdefinition)
                                   (equal key (list (first entry) (second entry)
                                                    (third entry)))))
                            allowances)))
            (if allowance
                (progn
                  (setf (gethash allowance used) t)
                  (unless (%handwritten-allowance-reason-p allowance 3)
                    (push (format nil "allowance for dynamic FDEFINITION at ~A:~D requires a nonempty reason"
                                  (second candidate) (third candidate))
                          violations)))
                (push (format nil "~A:~D: dynamic FDEFINITION target"
                              (second candidate) (third candidate))
                      violations)))
          (let* ((key (subseq candidate 0 2))
                 (allowance (%handwritten-allowance key allowances)))
            (if allowance
                (progn
                  (setf (gethash allowance used) t)
                  (unless (%handwritten-allowance-reason-p allowance)
                    (push (format nil "allowance for ~A:~A requires a nonempty reason"
                                  (first key) (second key))
                          violations)))
                (push (format nil "~A:~D: hand-written replacement of ~A:~A"
                              (third candidate) (fourth candidate)
                              (first key) (second key))
                      violations)))))
    (dolist (allowance allowances)
      (let* ((dynamic-p (and (consp allowance)
                             (eq (first allowance) :dynamic-fdefinition)))
             (key
               (cond
                 ((and dynamic-p
                       (stringp (second allowance))
                       (integerp (third allowance)))
                  (list :dynamic-fdefinition (second allowance) (third allowance)))
                 ((and (consp allowance) (consp (rest allowance))
                       (stringp (first allowance)) (stringp (second allowance)))
                  (list (string-upcase (first allowance))
                        (string-upcase (second allowance))))))
             (reason-index (if dynamic-p 3 2)))
        (cond
          ((null key)
           (push (format nil "invalid handwritten-stub allowance ~S" allowance)
                 violations))
          ((not (gethash allowance used))
           (push (if dynamic-p
                     (format nil "stale handwritten-stub allowance for dynamic FDEFINITION at ~A:~D"
                             (second allowance) (third allowance))
                     (format nil "stale handwritten-stub allowance for ~A:~A"
                             (first key) (second key)))
                 violations))
          ((not (%handwritten-allowance-reason-p allowance reason-index))
           nil))))
    (values (sort (nreverse violations) #'string<)
            (sort (remove-duplicates unresolved :test #'string=) #'string<))))
