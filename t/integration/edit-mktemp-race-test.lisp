(in-package #:aitools.edit.test)
(defun call-with-mktemp-boundaries (overrides function)
  (let* ((ports *ports*)
         (*ports*
           (aitools.edit.application:make-edit-ports
            :workspace-host (aitools.edit.application::edit-ports-workspace-host ports)
            :open-store
            (lambda (root)
              (aitools.store.application:make-store
               (apply (function cl-boundary-kit:boundary-context-with)
                      (aitools.store.infrastructure:make-posix-store-io :root root)
                      overrides)
               root *home* :temporary (%temporary-area-p root)))
            :text-source (aitools.edit.application::edit-ports-text-source ports)
            :read-stdin-octets
            (aitools.edit.application::edit-ports-read-stdin-octets ports)
            :unix-now (aitools.edit.application::edit-ports-unix-now ports)
            :serialize-document
            (aitools.edit.application::edit-ports-serialize-document ports))))
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
              (aitools.store.infrastructure:make-posix-store-io)
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
