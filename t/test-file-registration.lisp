(in-package #:aitools/test)

(defparameter *test-file-excluded-directories*
  '(("perf/" . "Performance tests use a separate runner.")
    ("integration/structure-budget-baselines/" . "Structure budget data is loaded by the structure tests.")
    ("integration/structure-context-allowances/" . "Structure allowance data is loaded by the structure tests.")
    ("spec-count-baselines/" . "Spec-count data is loaded by the baseline loader.")))

(defparameter *conditional-test-files*
  '(("unit/process/pidfd-test.lisp" (:and :linux (:or :x86-64 :arm64))
     "The pidfd syscall wrappers exist only on these Linux architectures.")
    ("integration/process-pidfd-test.lisp" (:and :linux (:or :x86-64 :arm64))
     "The pidfd integration test exercises those Linux wrappers.")))

;; Fixtures are loaded by the tests that use them.
(defun %excluded-test-file-p (path)
  (or (search "/fixtures/" (concatenate (quote string) "/" path))
      (some (lambda (entry) (uiop:string-prefix-p (car entry) path))
            *test-file-excluded-directories*)))

(defun %conditional-test-file-inactive-p (path features)
  (let ((entry (assoc path *conditional-test-files* :test (function string=))))
    (and entry (not (uiop:featurep (second entry) features)))))

(defun %test-file-registration-problems (files registrations features)
  (let ((problems nil))
    (dolist (file files)
      (unless (%excluded-test-file-p file)
        (let ((owners (loop for (path . system) in registrations
                             when (string= path file) collect system)))
          (cond ((cdr owners)
                 (push (format nil "test file ~A has multiple components in test systems: ~{~A~^, ~}"
                               file owners)
                       problems))
                ((and (null owners) (not (%conditional-test-file-inactive-p file features)))
                 (push (format nil "test file ~A is not registered in a test system" file)
                       problems))))))
    (nreverse problems)))

(defun %test-file-paths (directory root)
  (append (mapcar (lambda (path) (enough-namestring path root))
                  (uiop:directory-files directory "*.lisp"))
          (mapcan (lambda (subdirectory) (%test-file-paths subdirectory root))
                  (uiop:subdirectories directory))))

(defun %test-system-component-paths (component system root)
  (typecase component
    (asdf:cl-source-file
     (list (cons (enough-namestring (asdf:component-pathname component) root) system)))
    (asdf:module
     (mapcan (lambda (child) (%test-system-component-paths child system root))
             (asdf:component-children component)))))

(defun %validate-test-file-registration ()
  (let* ((root (merge-pathnames "t/" (asdf:system-source-directory "aitools/test/base")))
         (systems (append (uiop:symbol-call :asdf-user :%registered-aitools-test-systems)
                          (remove-if-not (lambda (name)
                                           (uiop:string-prefix-p "aitools/test-support/" name))
                                         (asdf:registered-systems))))
         (registrations
           (loop for system in systems
                 append (%test-system-component-paths (asdf:find-system system) system root)))
         (problems (%test-file-registration-problems
                    (%test-file-paths root root) registrations *features*)))
    (when problems
      (error "Test file registration failed: ~{~A~^; ~}" problems))
    t))
