;;;; run-tests.lisp
;;;;
;;;; Lisp-level test entry point:
;;;;
;;;;     sbcl --script run-tests.lisp
;;;;
;;;; Registers this checkout on ASDF's source registry, inherits the caller's
;;;; configuration for sibling dependencies (cl-cli, cl-weave, and every kit
;;;; -- set CL_SOURCE_REGISTRY to the nerima-lisp checkout root so ASDF can
;;;; find them), and runs the test system. Mirrors cl-cowsay's run-tests.lisp:
;;;; the exit goes through a HANDLER-CASE around ASDF:TEST-SYSTEM so a
;;;; failing suite is a named one-line diagnostic and a deliberate exit
;;;; rather than an escaping backtrace, and the zero exit means only "TEST-OP
;;;; signalled nothing" -- read the "successful completion" line for whether
;;;; any spec actually ran. See docs/src/project/development.md,
;;;; "Running the tests", for the environment it expects.

(require :asdf)

(defun script-directory ()
  (make-pathname :name nil
                 :type nil
                 :defaults (or *load-truename*
                               *compile-file-truename*
                               (error "Unable to determine the script location"))))

;; :DIRECTORY, not the :TREE sibling repositories use: aitools.asd names every
;; component by path, so only the root needs registering, and a recursive scan
;; would also pick up any `.asd` under a `result` link or a scratch directory
;; ahead of the real dependency.
(defun configure-local-source-registry (root)
  (asdf:initialize-source-registry
   `(:source-registry
     (:directory ,root)
     :inherit-configuration)))

(let ((root (script-directory)))
  (configure-local-source-registry root)
  (handler-case (asdf:test-system "aitools")
    (error (condition)
      (format *error-output* "~&aitools tests failed: ~A~%" condition)
      (uiop:quit 1)))
  (uiop:quit 0))
