(in-package #:aitools.process.integration-test)

(defun process-source-files (directory)
  (append
   (remove-if-not
    (lambda (path)
      (string-equal (pathname-type path) "lisp"))
    (uiop:directory-files directory))
   (mapcan #'process-source-files (uiop:subdirectories directory))))

(defun process-uses-old-boundary-symbol-p ()
  (some
   (lambda (source)
     (search (concatenate (quote string) "process-po" "rts") (uiop:read-file-string source) :test #'char-equal))
   (process-source-files #p"packages/feature/process/src/")))

(describe "aitools process boundary contract"
          (it "removes old process boundary symbols from production source"
              (expect (process-uses-old-boundary-symbol-p) :to-be nil))
          (it "registers every required application capability in composition"
              (let* ((keys
                      (append
                       '(:process-run-program :process-bg-directory
                                              :process-launch-detached
                                              :process-list-directory
                                              :process-read-file-text
                                              :process-read-file-octets
                                              :process-file-size
                                              :process-create-file-exclusive
                                              :process-replace-file
                                              :process-remove-file
                                              :process-group-alive-p)
                       '(:process-signal-group :process-list-pids
                                               :process-process-info
                                               :process-safe-target-p
                                               :process-signal-process
                                               :process-tcp-connectable-p
                                               :process-universal-time
                                               :process-monotonic-ms
                                               :process-sleep-ms
                                               :process-workspace-host
                                               :process-temporary-directory)))
                     (boundaries
                      (aitools.process.composition::make-process-dependencies))
                     (provided
                      (mapcar
                       (lambda (key)
                         (let ((capability
                                (cl-boundary-kit:boundary-context-require
                                 boundaries
                                 key)))
                           (unless (eq key :process-workspace-host)
                             (expect (functionp capability) :to-be t))
                           key))
                       keys)))
                (expect (length keys) :to-be 22)
                (expect provided :to-equal keys))))
