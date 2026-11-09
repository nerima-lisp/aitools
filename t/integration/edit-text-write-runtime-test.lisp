(in-package #:aitools.edit.test)

(describe "aitools in-process write runtime"
  (it "writes supplied bytes under a mktemp directory through the in-process write runtime"
    (call-with-edit-workspace (lambda () (let* ((temporary (mktemp-path :dir t))
             (destination (format nil "~A/decoded.bin" temporary))
             (decoded (octet-vector 0 97 0 255))
             (result nil))
        (aitools.edit.application:run-write-command/k
         *ports*
         (aitools.edit.application:make-write-plan
          :command "util.decode"
          :targets (list (aitools.edit.application:make-write-target destination))
          :inputs (list decoded)
          :record-options (lambda (paths) (list :to (first paths)))
          :record-positionals (lambda (paths) (declare (ignore paths)) (list "base64"))
          :plan (lambda (context &key on-committed on-rejected)
                  (declare (ignore on-rejected))
                  (funcall on-committed
                           (list (aitools.store.domain:write-file-request
                                  (aitools.edit.application:context-path context)
                                  decoded))
                           (list (cons "bytes" (length decoded))))))
         :root *root*
         :on-ok (lambda (fields) (declare (ignore fields)) (setf result :ok))
         :on-error (lambda (code message &rest keys)
                     (declare (ignore message keys))
                     (setf result (list :error code))))
        (expect result :to-be :ok)
        (with-open-file (input (sb-ext:parse-native-namestring destination)
                               :element-type (array-element-type decoded))
          (let ((actual (make-array (file-length input)
                                    :element-type (array-element-type decoded))))
            (read-sequence actual input)
            (expect actual :to-equalp decoded))))))))
(describe "aitools bounded write hash validation"
  (it "caps a regular-file read reached through a symlink"
    (call-with-edit-workspace
     (lambda ()
       (put "large.md" "12345")
       (sb-posix:symlink "large.md" (disk "link.md"))
       (let ((before (snapshot))
             (result nil))
         (aitools.edit.application:run-write-command/k
          *ports*
          (aitools.edit.application:make-write-plan
           :command "md.replace"
           :targets (list
                     (aitools.edit.application:make-write-target "link.md"))
           :expect-hashes
           (list (format nil "link.md=~A" (hash "large.md")))
           :max-target-bytes 4
           :plan (lambda (context &key on-committed on-rejected)
                   (declare (ignore context on-rejected))
                   (funcall on-committed (list))))
          :root *root*
          :on-ok (lambda (fields)
                   (declare (ignore fields))
                   (setf result :ok))
          :on-error (lambda (code message &rest keys)
                      (declare (ignore message keys))
                      (setf result (list :error code))))
         (expect result :to-equal (list :error "refusal.too-large"))
         (expect (snapshot) :to-equal before)))))
  (it "requires a store view for bounded path hashes"
    (call-with-edit-workspace
     (lambda ()
       (put "large.md" "12345")
       (let* ((host
                (aitools.workspace.infrastructure:make-host-workspace-boundaries))
              (path (disk "large.md"))
              (unbounded
                (aitools.store.application:path-hash host *root* nil path)))
         (expect unbounded :to-equal (hash "large.md"))
         (expect
          (handler-case
              (progn
                (aitools.store.application:path-hash
                 host *root* nil path :max-bytes 4)
                nil)
            (type-error (condition)
              (type-error-expected-type condition)))
          :to-equal 'aitools.store.application:store-view))))))
