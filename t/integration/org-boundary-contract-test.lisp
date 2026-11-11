(in-package #:aitools.integration.org-dispatch-test)

(defun %org-boundary-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/org")))

(defun %org-boundary-lisp-files (directory)
  (append
   (remove-if-not
    (lambda (pathname)
      (string-equal (or (pathname-type pathname) "") "lisp"))
    (uiop:directory-files directory))
   (mapcan (function %org-boundary-lisp-files)
           (uiop:subdirectories directory))))

(defun %org-boundary-keys-in-form (form)
  (when (consp form)
    (let ((key (and (consp (cdr form)) (consp (cddr form)) (third form))))
      (append
       (when (and (eq (first form)
                      (quote cl-boundary-kit:boundary-context-require))
                  (keywordp key)
                  (>= (length (symbol-name key)) 4)
                  (string= (subseq (symbol-name key) 0 4) "ORG-"))
         (list key))
       (%org-boundary-keys-in-form (car form))
       (%org-boundary-keys-in-form (cdr form))))))

(defun %org-required-boundary-keys ()
  (let ((keys (list))
        (*package* (find-package (quote #:aitools.org.application)))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file
             (%org-boundary-lisp-files
              (merge-pathnames "application/"
                               (%org-boundary-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys
                       (nconc keys
                              (%org-boundary-keys-in-form form))))))
    (remove-duplicates keys :test (function eq))))

(describe "aitools org boundary contract"
  (it "contains no retired production symbol or ports source file"
    (let* ((source-files
             (%org-boundary-lisp-files (%org-boundary-source-directory)))
           (legacy-symbol-files
             (loop for file in source-files
                   when (search "org-ports" (uiop:read-file-string file)
                                :test (function char-equal))
                     collect file))
           (legacy-port-files
             (remove-if-not
              (lambda (file)
                (string-equal (or (pathname-name file) "") "ports"))
              source-files)))
      (expect source-files :to-be-truthy)
      (expect legacy-symbol-files :to-equal nil)
      (expect legacy-port-files :to-equal nil)))
  (it "registers each capability required by application flows"
    (let* ((required-keys (%org-required-boundary-keys))
           (expected-keys
             (list :org-workspace-host :org-text-source :org-max-input-bytes
                   :org-parse :org-outline :org-query :org-export
                   :org-open-store :org-write :org-edit :org-agenda
                   :org-clock-report))
           (workspace-host
             (aitools.workspace.infrastructure:make-host-workspace-boundaries))
           (text-source (aitools.text.infrastructure:make-host-text-source))
           (open-store (function identity))
           (boundaries
             (aitools.org.composition::make-org-dependencies
              :workspace-host workspace-host
              :text-source text-source
              :open-store open-store)))
      (expect required-keys :to-be-truthy)
      (expect
       (every (lambda (key)
                (member key expected-keys :test (function eq)))
              required-keys)
       :to-be t)
      (expect
       (every (lambda (key)
                (member key required-keys :test (function eq)))
              expected-keys)
       :to-be t)
      (expect
       (every (lambda (key)
                (cl-boundary-kit:boundary-context-present-p boundaries key))
              required-keys)
       :to-be t)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :org-workspace-host)
              :to-be workspace-host)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :org-text-source)
              :to-be text-source)
      (expect (integerp
               (cl-boundary-kit:boundary-context-require
                boundaries :org-max-input-bytes))
              :to-be t)
      (dolist (key
               (list :org-open-store :org-parse :org-outline :org-query
                     :org-export :org-write :org-edit :org-agenda
                     :org-clock-report))
        (expect
         (functionp
          (cl-boundary-kit:boundary-context-require boundaries key))
         :to-be t)))))
