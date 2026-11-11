(in-package #:aitools.edit.test)
(describe "aitools mktemp area boundary"
  (it "accepts an in-workspace temporary area outside protected metadata"
    (call-with-edit-workspace
     (lambda ()
       (let ((temporary-root (disk "safe-area/tmp")))
         (cl-weave:with-mocked-functions
             (((symbol-function 'aitools.store.domain:tmp-directory)
               (lambda (state-directory)
                 (declare (ignore state-directory))
                 temporary-root)))
           (multiple-value-bind (kind fields) (run "mktemp" '())
             (expect kind :to-be :ok)
             (let ((path (field fields "path")))
               (expect (search temporary-root path) :to-equal 0)
               (expect (probe-file path) :to-be-truthy)))))))))
(defun call-with-mktemp-boundaries (overrides function)
  (let* ((boundaries *boundaries*)
         (*boundaries*
           (make-edit-test-boundaries :edit-workspace-host (cl-boundary-kit:boundary-context-require boundaries :edit-workspace-host)
            :edit-open-store
            (lambda (root)
              (aitools.store.application:make-store
               (apply (function cl-boundary-kit:boundary-context-with)
                      (aitools.store.infrastructure:make-posix-store-io :root root)
                      overrides)
               root *home* :temporary (%temporary-area-p root)))
            :edit-text-source (cl-boundary-kit:boundary-context-require boundaries :edit-text-source)
            :edit-read-stdin-octets
            (cl-boundary-kit:boundary-context-require boundaries :edit-read-stdin-octets)
            :edit-unix-now (cl-boundary-kit:boundary-context-require boundaries :edit-unix-now)
            :edit-serialize-document
            (cl-boundary-kit:boundary-context-require boundaries :edit-serialize-document))))
    (funcall function)))

(describe "aitools mktemp against a racing mktemp area"
  (it "draws another name when the first is taken"
  (call-with-edit-workspace
   (lambda ()
     (let* ((first (mktemp-path))
            (tmp (subseq first 0 (position #\/ first :from-end t)))
            (taken (concatenate (quote string) tmp "/tmp.aaaaaaaaaaaa"))
            (draws (list "aaaaaaaaaaaa" "bbbbbbbbbbbb")))
       (with-open-file (out (sb-ext:parse-native-namestring taken)
                            :direction :output :if-does-not-exist :create)
         (write-string "kept" out))
       (call-with-mktemp-boundaries
        (list :random-hex
              (lambda (count)
                (declare (ignore count))
                (pop draws)))
        (lambda ()
          (expect (mktemp-path)
                  :to-equal (concatenate (quote string) tmp "/tmp.bbbbbbbbbbbb"))))
       (expect (with-open-file (in (sb-ext:parse-native-namestring taken))
                 (read-line in))
               :to-equal "kept")))))

  (it "accepts the area another process created between its check and its mkdir"
  (call-with-edit-workspace
   (lambda ()
     (let ((real-mkdir
             (cl-boundary-kit:boundary-context-require
              (aitools.store.infrastructure:make-posix-store-io :root *root*)
              :mkdir)))
       (call-with-mktemp-boundaries
        (list :mkdir
              (lambda (path &rest keys)
                (apply real-mkdir path keys)
                (if (search "/tmp" path :from-end t
                            :start2 (max 0 (- (length path) 4)))
                    (error (quote aitools.store.application:store-io-error)
                           :operation "mkdir" :path path
                           :detail "File exists")
                    nil)))
        (lambda ()
          (let ((path (mktemp-path)))
            (expect (probe-file path) :to-be-truthy)))))))))
