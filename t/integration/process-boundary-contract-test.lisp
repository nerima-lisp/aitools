(in-package #:aitools.process.integration-test)

(defun process-source-files (directory)
  (append
   (remove-if-not (lambda (path) (string-equal (pathname-type path) "lisp"))
                  (uiop:directory-files directory))
   (mapcan #'process-source-files (uiop:subdirectories directory))))

(defun process-uses-old-ports-symbol-p ()
  (some (lambda (source)
          (search "process-ports" (uiop:read-file-string source) :test #'char-equal))
        (process-source-files #p"packages/feature/process/src/")))

(describe "aitools process boundary contract"
  (it "removes old process-ports symbols from production source"
    (expect (process-uses-old-ports-symbol-p) :to-be nil))

  (it "registers every required application capability in composition"
    (let* ((keys aitools.process.application::+process-boundary-keys+)
           (boundaries
             (aitools.process.composition::make-process-dependencies))
           (provided
             (mapcar (lambda (key)
                       (let ((capability
                               (cl-boundary-kit:boundary-context-require boundaries key)))
                         (unless (eq key :process-workspace-host)
                           (expect (functionp capability) :to-be t))
                         key))
                     keys)))
      (expect (length keys) :to-be 22)
      (expect provided :to-equal keys))))
