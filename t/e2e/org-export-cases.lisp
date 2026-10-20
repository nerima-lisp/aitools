;;;; t/e2e/org-export-cases.lisp
(in-package #:aitools.e2e.test)

(defparameter +org-export-source-mode-fixture+
  (format nil
          (concatenate 'string
                       "#+BEGIN_SRC text~%source-default-marker~%#+END_SRC~%~%"
                       "#+RESULTS:~%: result-default-marker~%~%"
                       "#+BEGIN_SRC text :exports code~%source-code-marker~%#+END_SRC~%~%"
                       "#+RESULTS:~%: result-code-marker~%~%"
                       "#+BEGIN_SRC text :exports results~%source-results-marker~%#+END_SRC~%~%"
                       "#+RESULTS:~%: result-results-marker~%~%"
                       "#+BEGIN_SRC text :exports both~%source-both-marker~%#+END_SRC~%~%"
                       "#+RESULTS:~%: result-both-marker~%~%"
                       "#+BEGIN_SRC text :exports none~%source-none-marker~%#+END_SRC~%~%"
                       "#+RESULTS:~%: result-none-marker~%")))

(defparameter +org-export-source-mode-expectations+
  '(("source-default-marker" "result-default-marker" t nil)
    ("source-code-marker" "result-code-marker" t nil)
    ("source-results-marker" "result-results-marker" nil t)
    ("source-both-marker" "result-both-marker" t t)
    ("source-none-marker" "result-none-marker" nil nil)))

(defparameter +org-export-invalid-source-mode-fixtures+
  `(("unknown" . ,(format nil "#+BEGIN_SRC text :exports html~%code~%#+END_SRC~%"))
    ("duplicate" . ,(format nil "#+BEGIN_SRC text :exports code :exports none~%code~%#+END_SRC~%"))
    ("value-less" . ,(format nil "#+BEGIN_SRC text :exports~%code~%#+END_SRC~%"))))

(describe "aitools e2e org source block export modes"
  (define-org-read-case ("org export applies source block exports modes in every backend" ws)
    (put ws "source-modes.org" +org-export-source-mode-fixture+)
    (let ((before (file-octets ws "source-modes.org")))
      (dolist (backend (list "html" "gfm" "ascii"))
        (let* ((envelope (org-ok ws
                                 (list "org" "export" "source-modes.org" "--backend" backend)
                                 "org export"))
               (content (jget envelope "content")))
          (expect (jget envelope "backend") :to-equal backend)
          (expect (stringp content) :to-be t)
          (dolist (expectation +org-export-source-mode-expectations+)
            (destructuring-bind (source result code-p results-p) expectation
              (expect (not (null (search source content))) :to-be code-p)
              (expect (not (null (search result content))) :to-be results-p)))))
      (expect (file-octets ws "source-modes.org") :to-equalp before)))
  (define-org-read-case ("org export rejects invalid source block exports modes without content" ws)
    (dolist (fixture +org-export-invalid-source-mode-fixtures+)
      (put ws "invalid-exports.org" (cdr fixture))
      (let ((before (file-octets ws "invalid-exports.org")))
        (dolist (backend (list "html" "gfm" "ascii"))
          (let* ((envelope (org-error ws
                                      (list "org" "export" "invalid-exports.org"
                                            "--backend" backend)
                                      "org export" "input.unsupported-format"))
                 (present (nth-value 1 (gethash "content" envelope))))
            (expect present :to-be nil))
          (expect (file-octets ws "invalid-exports.org") :to-equalp before)))))
)
