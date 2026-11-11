(in-package #:aitools.inspect.test)

(defun %uncovered-application-context (boundaries)
  (aitools.inspect.application::%make-inspect-context
   :boundaries boundaries
   :root (aitools.workspace.domain:make-workspace-root
          :path "/work" :real "/work")))

(describe "inspect application flow uncovered paths"
  (it "treats a symlink returned by host stat as absent"
    (let* ((context
             (%uncovered-application-context (make-test-boundaries)))
           (entry
             (aitools.workspace.application:make-workspace-entry
              :name "link" :kind :symlink :size 0 :mtime 0 :mode #o777))
           (target nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.workspace.application:resolve-real-path))
            (lambda (host path)
              (declare (ignore host))
              path))
           ((symbol-function
             (quote aitools.workspace.application:host-stat))
            (lambda (host path)
              (declare (ignore host path))
              entry)))
        (setf target
              (aitools.inspect.application::probe-target context "link")))
      (expect (aitools.inspect.application::file-target-kind target)
              :to-be :absent)))

  (it "returns an empty text result for an exclusive adjacent-line selection"
    (multiple-value-bind (kind fields)
        (run-flow #'aitools.inspect.application:read-flow
                  (make-test-boundaries
                   :files `(("/work/adjacent.txt" .
                             ,(format nil "first~%start~%end~%last~%"))))
                  "adjacent.txt"
                  :between '("start" "end")
                  :exclusive t)
      (expect kind :to-be :ok)
      (expect (field fields "start_line") :to-be 3)
      (expect (field fields "lines") :to-equal '())))

  (it "joins a printable string that crosses a chunk boundary"
    (let* ((prefix
             (make-array 65535
                         :element-type '(unsigned-byte 8)
                         :initial-element 0))
           (bytes
             (concatenate '(simple-array (unsigned-byte 8) (*))
                          prefix
                          (%as-octets "cross-boundary")
                          (vector 0))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.inspect.application:read-flow
                    (make-test-boundaries :files `(("/work/strings.bin" . ,bytes)))
                    "strings.bin" :as "strings" :min-length 4)
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1)
        (expect (json-object-get (first (field fields "strings")) "offset")
                :to-be 65535)
        (expect (json-object-get (first (field fields "strings")) "text")
                :to-equal "cross-boundary"))))

  (it "forwards a successful snapshot comparison"
    (let ((snapshot
            (aitools.inspect.domain:make-snapshot
             "snap-20231114T221320Z-0a1b2c3d" "2023-11-14T22:13:20Z" '())))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.inspect.application::%load-snapshot/k))
            (lambda (context id &key on-snapshot on-error)
              (declare (ignore context id on-error))
              (funcall on-snapshot snapshot))))
        (multiple-value-bind (kind fields)
            (run-flow #'aitools.inspect.application:snapshot-diff-flow
                      (make-test-boundaries) "snap-20231114T221320Z-0a1b2c3d")
          (expect kind :to-be :ok)
          (expect (field fields "snapshot_id")
                  :to-equal "snap-20231114T221320Z-0a1b2c3d")
          (expect (field fields "added") :to-equal '())
          (expect (field fields "removed") :to-equal '()))))))
