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
              (aitools.protocol.application:all-command-schemas registry)))
           (names
            (mapcar (function aitools.protocol.domain:command-schema-name)
                    schemas)))
      (expect schemas :to-be-truthy)
      (when (find-package "AITOOLS.VCS.PRESENTATION")
        (let ((git
               (position-if
                (lambda (name)
                  (and (>= (length name) 4) (string= "git." name :end2 4)))
                names))
              (org
               (position-if
                (lambda (name)
                  (and (>= (length name) 4) (string= "org." name :end2 4)))
                names)))
          (expect (and git org (< git org)) :to-be t))))))
