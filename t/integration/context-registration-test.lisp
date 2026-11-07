(in-package #:aitools/test)

(defun %feature-presentation-context (system-name)
  (when (stringp system-name)
    (let* ((prefix "aitools/feature/")
           (suffix "/presentation")
           (suffix-start (- (length system-name) (length suffix))))
      (when (and (>= suffix-start (length prefix))
                 (string= prefix system-name :end2 (length prefix))
                 (string= suffix system-name :start2 suffix-start))
        (intern (string-upcase (subseq system-name (length prefix) suffix-start))
                :keyword)))))

(defun %declared-feature-presentation-contexts ()
  (loop for system-name in (asdf:system-depends-on
                            (asdf:find-system "aitools/cli"))
        for context = (%feature-presentation-context system-name)
        when context
          collect context))

(describe "Feature command registration"
  (it "registers every feature presentation in CLI dependency order"
    (let ((expected (%declared-feature-presentation-contexts))
          (actual
           (mapcar (function first)
                   (aitools/cli::context-command-registrations))))
      (expect expected :to-be-truthy)
      (expect actual :to-equal expected)))
  (it "registers commands from each context dependency constructor"
    (let* ((registry (aitools.protocol.application:make-command-registry))
       (schemas
        (progn
          (aitools/cli:register-all-context-commands registry)
          (aitools.protocol.application:all-command-schemas registry))))
  (expect schemas :to-be-truthy))))
