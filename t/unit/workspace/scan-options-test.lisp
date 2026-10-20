(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application scan option normalization"
  (it "keeps the default size limit when scan options are omitted"
    (let* ((host (make-fake-host))
           (options
             (call-with-scan-options/k
              host
              :path-absolute (lambda (path) (declare (ignore path)) nil)
              :current-time (constantly 10000)
              :language-predicate (lambda (language) (declare (ignore language)) nil)
              :language-names '("common-lisp")
              :on-options #'identity
              :on-error (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (fail "unexpected scan option error")))))
      (expect (getf options :skip-larger-than) :to-be +default-skip-larger-than+)
      (expect (getf options :newer) :to-be nil)
      (expect (getf options :lang) :to-be nil)
      (expect (getf options :glob) :to-be nil)
      (expect (getf options :no-ignore) :to-be nil)))

  (it "normalizes a size, duration, and known language before calling ON-OPTIONS"
    (let* ((host (make-fake-host))
           (overlay (make-workspace-overlay))
           (options
             (call-with-scan-options/k
              host
              :path-absolute (lambda (path) (declare (ignore path)) nil)
              :current-time (constantly 10000)
              :language-predicate
              (lambda (language)
                (when (string= language "common-lisp")
                  (lambda (path) (search ".lisp" path))))
              :language-names '("common-lisp" "markdown")
              :glob '("*.lisp")
              :lang "common-lisp"
              :no-ignore t
              :skip-larger-than "1KiB"
              :newer "1h"
              :overlay overlay
              :on-options #'identity
              :on-error (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (fail "unexpected scan option error")))))
      (expect (getf options :skip-larger-than) :to-be 1024)
      (expect (getf options :newer) :to-be 6400)
      (expect (funcall (getf options :lang) "main.lisp") :to-be-truthy)
      (expect (getf options :glob) :to-equal '("*.lisp"))
      (expect (getf options :no-ignore) :to-be t)
      (expect (getf options :overlay) :to-be overlay)))

  (it "uses the mtime of an existing duration-shaped path"
    (let* ((host (make-fake-host :files '(("/w/7d" . "reference")) :cwd "/w"))
           (options
             (call-with-scan-options/k
              host
              :path-absolute (lambda (path) (join-path "/w" path))
              :current-time (lambda () (fail "an existing path is not a duration"))
              :language-predicate (lambda (language) (declare (ignore language)) nil)
              :language-names '("common-lisp")
              :newer "7d"
              :on-options #'identity
              :on-error (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (fail "unexpected scan option error")))))
      (expect (getf options :newer) :to-be 1000)))

  (it "reports an invalid size before resolving newer paths or language names"
    (let ((resolved-path nil)
          (continued nil)
          (host (make-fake-host)))
      (expect
       (call-with-scan-options/k
        host
        :path-absolute (lambda (path) (setf resolved-path path))
        :current-time (lambda () (fail "invalid size stops option parsing"))
        :language-predicate (constantly nil)
        :language-names '("common-lisp")
        :lang "cobol"
        :skip-larger-than "huge"
        :newer "yesterday"
        :on-options (lambda (options) (setf continued options))
        :on-error (lambda (kind value names) (list kind value names)))
       :to-equal '(:invalid-size "huge" nil))
      (expect resolved-path :to-be-falsy)
      (expect continued :to-be-falsy)))

  (it "reports a newer value that is neither a path nor a duration"
    (let ((host (make-fake-host)))
      (expect
       (call-with-scan-options/k
        host
        :path-absolute (lambda (path) (declare (ignore path)) nil)
        :current-time (lambda () (fail "invalid duration stops option parsing"))
        :language-predicate (lambda (language) (declare (ignore language)) nil)
        :language-names '("common-lisp")
        :newer "yesterday"
        :on-options (lambda (options) (declare (ignore options)) (fail "invalid newer called ON-OPTIONS"))
        :on-error (lambda (kind value names) (list kind value names)))
       :to-equal '(:invalid-newer "yesterday" nil))))

  (it "reports an unknown language with the known names"
    (let ((host (make-fake-host)))
      (expect
       (call-with-scan-options/k
        host
        :path-absolute (lambda (path) (declare (ignore path)) nil)
        :current-time (constantly 10000)
        :language-predicate (constantly nil)
        :language-names '("common-lisp" "markdown")
        :lang "cobol"
        :on-options (lambda (options) (declare (ignore options)) (fail "unknown language called ON-OPTIONS"))
        :on-error (lambda (kind value names) (list kind value names)))
       :to-equal '(:unknown-language "cobol" ("common-lisp" "markdown"))))))
