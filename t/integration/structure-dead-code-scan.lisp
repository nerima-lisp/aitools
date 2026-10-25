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

(defun %liveness-export-reference-p (references kind package namespace symbol)
  (or (gethash (list kind package namespace symbol) references)
      (and (eq namespace :function)
           (gethash (list kind package :function
                          (format nil "(SETF ~A)" symbol))
                    references))))
(defparameter *liveness-dead-code-allowlist*
  (list
   (list :finding "packages/core/kernel/src/domain/flow.lisp:68: unused definition AITOOLS.KERNEL.DOMAIN:%CHECK-FLOW-CONTINUATIONS"
         :reason "DEFINE-FLOW emits this call at packages/core/kernel/src/domain/flow.lisp:103; the static walker excludes quasiquoted generated forms.")
   (list :finding "packages/core/protocol/src/application/package.lisp:3: exported symbol AITOOLS.PROTOCOL.APPLICATION:MAKE-COMMAND-DECLARATION is referenced only from tests"
         :reason "DEFINE-COMMAND emits this constructor call at packages/core/protocol/src/application/command-registry.lisp:125; its quasiquote hides the production reference.")
   (list :finding "packages/core/store/src/application/files.lisp:110: unused definition AITOOLS.STORE.APPLICATION:CALL-WITH-TEMP-DIR"
         :reason "WITH-TEMP-DIR emits this call at packages/core/store/src/application/files.lisp:124; production code uses the macro.")
   (list :finding "packages/core/store/src/application/files.lisp:94: unused definition AITOOLS.STORE.APPLICATION:CALL-WITH-TEMP-FILE"
         :reason "WITH-TEMP-FILE emits this call at packages/core/store/src/application/files.lisp:108; production code uses the macro.")
   (list :finding "packages/core/store/src/application/write-preparation.lisp:39: unused definition AITOOLS.STORE.APPLICATION:CALL-WITH-PREPARED-INTENT/K"
         :reason "WITH-PREPARED-INTENT emits this call at packages/core/store/src/application/write-preparation.lisp:78; production write planning invokes the macro.")
   (list :finding "packages/core/text/src/domain/package.lisp:14: exported symbol AITOOLS.TEXT.DOMAIN:MAP-LINES is referenced only from tests"
         :reason "DO-LINES emits this call at packages/core/text/src/domain/line-index.lisp:49; production BUILD-LINE-INDEX uses the macro.")
   (list :finding "packages/feature/edit/src/application/yaml-toml-command-flows.lisp:141: unused definition AITOOLS.EDIT.APPLICATION:%FORMAT-COMMAND/K"
         :reason "DEFINE-FORMAT-WRITE-COMMANDS emits this call at packages/feature/edit/src/application/yaml-toml-command-flows.lisp:197; production YAML and TOML flows invoke that macro.")
   (list :finding "packages/feature/inspect/src/presentation/registry.lisp:49: unused definition AITOOLS.INSPECT.PRESENTATION:%SCHEMA"
         :reason "DEFINE-INSPECT-COMMAND emits this call at packages/feature/inspect/src/presentation/registry.lisp:24; production command declarations invoke the macro.")
   (list :finding "packages/feature/util/src/domain/calc-parser.lisp:3: unused definition AITOOLS.UTIL.DOMAIN:%PARSER-DEPTH"
         :reason "%WITH-DEPTH emits these accessor calls at packages/feature/util/src/domain/calc-parser.lisp:25 and 27; production parser forms use the macro.")
   (list :finding "src/trace-context.lisp:4: unused definition AITOOLS/CLI:%TRACE-SESSION-PROVIDER"
         :reason "The DEFSTRUCT slot is written through SETF at src/trace-context.lisp:45; the analyzer separates the generated reader from its live setter."))
  "Exact findings accepted with an explanation of the analyzer limitation.")
(defun %liveness-filter-allowlisted-findings
    (findings &optional (allowlist *liveness-dead-code-allowlist*))
  (dolist (entry allowlist)
    (unless (and (listp entry)
                 (= (length entry) 4)
                 (eq (first entry) :finding)
                 (stringp (second entry))
                 (eq (third entry) :reason)
                 (stringp (fourth entry))
                 (plusp (length (string-trim '(#\Space #\Tab #\Newline)
                                             (fourth entry)))))
      (error "Dead-code allowlist entries require :finding and non-empty :reason: ~S"
             entry)))
  (remove-if (lambda (finding)
               (some (lambda (entry)
                       (string= finding (second entry)))
                     allowlist))
             findings))

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
                               (or (gethash (list kind
                                                  (getf definition :package)
                                                  (getf definition :namespace)
                                                  (getf definition :name))
                                            references)
                                   (and (getf definition :group)
                                        (eq (getf definition :namespace) :function)
                                        (gethash (list kind
                                                       (getf definition :package)
                                                       :function
                                                       (format nil "(SETF ~A)"
                                                               (getf definition :name)))
                                                 references))))
                 (exported-p (definition)
                   (member (%liveness-index-function-symbol-name
                            (getf definition :namespace)
                            (getf definition :name))
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
                                    (%liveness-export-reference-p references :test package namespace symbol))
                                  '(:function :value :type))
                            (not (some (lambda (namespace)
                                         (%liveness-export-reference-p references :production package namespace symbol))
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
          (%liveness-filter-allowlisted-findings
 (sort (remove-duplicates violations :test #'string=) #'string<)))))))
