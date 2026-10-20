(in-package #:aitools.edit.test)

(describe "aitools mktemp against a racing mktemp area"
  (it "draws another name when the first is taken"
    (with-workspace ()
      (let* ((first (mktemp-path))
             (tmp (subseq first 0 (position #\/ first :from-end t)))
             (taken (concatenate 'string tmp "/tmp.aaaaaaaaaaaa"))
             (draws (list "aaaaaaaaaaaa" "bbbbbbbbbbbb")))
        (with-open-file (out (sb-ext:parse-native-namestring taken) :direction :output :if-does-not-exist :create)
          (write-string "kept" out))
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.store.application:store-io-random-hex)
              (lambda (io)
                (declare (ignore io))
                (lambda (count)
                  (declare (ignore count))
                  (pop draws)))))
          (expect (mktemp-path)
                  :to-equal (concatenate 'string tmp "/tmp.bbbbbbbbbbbb")))
        (expect (with-open-file (in (sb-ext:parse-native-namestring taken)) (read-line in)) :to-equal "kept"))))

  (it "accepts the area another process created between its check and its mkdir"
    (with-workspace ()
      (let ((real-mkdir (symbol-function 'aitools.store.application:store-io-mkdir)))
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.store.application:store-io-mkdir)
              (lambda (io)
                (let ((mkdir (funcall real-mkdir io)))
                  (lambda (path &rest keys)
                    (apply mkdir path keys)
                    (if (search "/tmp" path :from-end t
                                :start2 (max 0 (- (length path) 4)))
                        (error 'aitools.store.application:store-io-error
                               :operation "mkdir" :path path
                               :detail "File exists")
                        nil))))))
          (let ((path (mktemp-path)))
            (expect (probe-file path) :to-be-truthy)))))))
