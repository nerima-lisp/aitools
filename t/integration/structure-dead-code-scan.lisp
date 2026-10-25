(in-package #:aitools.integration.structure-test)

(defun %liveness-package-token-used-p (records package symbol)
  "Whether production code in PACKAGE refers to SYMBOL without qualification."
  (some (lambda (record)
          (and (eq (getf record :kind) :production)
               (string= (getf record :package) package)
               (member symbol (getf record :tokens) :test #'string=)))
        records))

(defun %liveness-production-package-p (info)
  "True for package metadata declared under production source paths."
  (let ((file (getf info :file)))
    (and file
         (or (and (>= (length file) 9) (string= file "packages/" :end1 9))
             (and (>= (length file) 4) (string= file "src/" :end1 4))))))

(defun %liveness-documented-api-root-p (package namespace symbol)
  "Architecture documents these public APIs without in-tree callers."
  (and (eq namespace :function)
       (member (list package symbol)
               (quote (;; architecture.md:320 documents the public diff-generation API.
                       ("AITOOLS.KERNEL.DOMAIN" "GENERATE-UNIFIED-DIFF")
                       ;; architecture.md:251 documents the transaction lock macros.
                       ("AITOOLS.STORE.APPLICATION" "WITH-TX-LOCK")
                       ("AITOOLS.STORE.APPLICATION" "WITH-WORKSPACE-LOCK")
                       ;; architecture.md:250 documents the text boundary macro.
                       ("AITOOLS.TEXT.INFRASTRUCTURE" "WITH-TEXT-BOUNDARIES")
                       ;; architecture.md:249 documents the workspace boundary macro.
                       ("AITOOLS.WORKSPACE.INFRASTRUCTURE" "WITH-WORKSPACE-BOUNDARIES")))
               :test (function equal))))

(defun scan-dead-code-tree (root)
  "Sorted dead definitions and package violations under ROOT.
Exported definitions are public roots; test-only exports remain violations."
  (let* ((records (progn (clrhash *liveness-reader-view-cache*) (%liveness-files root)))
         (packages (%liveness-all-package-metadata records))
         (definitions (%liveness-definitions records))
         (violations '()))
    (multiple-value-bind (references local-references dynamic-lookups)
        (%liveness-reference-index records packages)
      (let ((live-structs (make-hash-table :test #'equal))
            (reported-structs (make-hash-table :test #'equal)))
        (labels ((referenced-p (definition kind)
                   (gethash (list kind
                                  (getf definition :package)
                                  (getf definition :namespace)
                                  (getf definition :name))
                            references))
                 (exported-p (definition)
                   (member (getf definition :name)
                           (getf (gethash (getf definition :package) packages)
                                 :exports)
                           :test #'string=))
                 (rooted-p (definition)
                   (or (getf definition :root)
                       (exported-p definition)))
                 (unused-definition (definition)
                   (format nil "~A:~D: unused definition ~A:~A"
                           (getf definition :file) (getf definition :line)
                           (getf definition :package) (getf definition :name))))
          (dolist (definition definitions)
            (let ((group (getf definition :group)))
              (when group
                (let ((package (getf definition :package))
                      (type-name (fourth group)))
                  (when (or (rooted-p definition)
                            (referenced-p definition :production))
                    (setf (gethash group live-structs) t
                          (gethash (list :production package :type type-name)
                                   references) t))
                  (when (referenced-p definition :test)
                    (setf (gethash group live-structs) t
                          (gethash (list :test package :type type-name)
                                   references) t))))))
          (dolist (definition definitions)
            (let* ((group (getf definition :group))
                   (namespace (getf definition :namespace)))
              (cond
                ((and group (eq namespace :type))
                 (unless (or (gethash group live-structs)
                             (gethash group reported-structs))
                   (setf (gethash group reported-structs) t)
                   (push (unused-definition definition) violations)))
                (group
                 (when (and (gethash group live-structs)
                            (not (rooted-p definition))
                            (not (referenced-p definition :production))
                            (not (referenced-p definition :test)))
                   (push (unused-definition definition) violations)))
                (t
                 (unless (or (rooted-p definition)
                             (referenced-p definition :production)
                             (referenced-p definition :test))
                   (push (unused-definition definition) violations))))))
          (setf violations (append dynamic-lookups violations))
          (maphash
           (lambda (package info)
             (when (%liveness-production-package-p info)
               (dolist (symbol (getf info :exports))
                 (when (and (not (%liveness-documented-api-root-p package :function symbol))
                            (some (lambda (namespace)
                                    (gethash (list :test package namespace symbol)
                                             references))
                                  '(:function :value :type))
                            (not (some (lambda (namespace)
                                         (gethash (list :production package namespace symbol)
                                                  references))
                                       '(:function :value :type))))
                   (push (format nil
                                 "~A:~D: exported symbol ~A:~A is referenced only from tests"
                                 (getf info :file) (getf info :line) package symbol)
                         violations)))
               (dolist (import (getf info :imports))
                 (unless (or (member (second import) (getf info :exports)
                                     :test #'string=)
                             (gethash (list :production package (second import))
                                      local-references))
                   (push (format nil "~A:~D: unused ~(~S~) ~A:~A"
                                 (getf info :file) (third import) (fourth import)
                                 (first import) (second import))
                         violations)))
               (dolist (use (getf info :uses))
                 (let ((used-package (gethash (first use) packages)))
                   (unless (some (lambda (symbol)
                                   (gethash (list :production package symbol)
                                            local-references))
                                 (getf used-package :exports))
                     (push (format nil "~A:~D: unused :use ~A"
                                   (getf info :file) (second use) (first use))
                           violations))))))
           packages)
          (sort (remove-duplicates violations :test #'string=) #'string<))))))
