(in-package #:aitools.text.test)

(describe "text range boundary operation"
  (it "returns a missing result for an absent path"
    (aitools.test.support:call-with-workspace
     (lambda (root state)
       (declare (ignore state))
       (multiple-value-bind (range size)
           (funcall (cl-boundary-kit:boundary-context-require
                     (aitools.text.infrastructure:make-host-text-source)
                     :text-read-range)
                    (format nil "~A/absent" root) 0 4)
         (expect range :to-be nil)
         (expect size :to-be nil)))))
  (it "returns an empty range and the file size"
    (aitools.test.support:call-with-workspace
     (lambda (root state)
       (declare (ignore state))
       (let ((path (format nil "~A/data" root)))
         (aitools.test.support:write-bytes path #(1 2 3))
         (multiple-value-bind (range size)
             (funcall (cl-boundary-kit:boundary-context-require
                       (aitools.text.infrastructure:make-host-text-source)
                       :text-read-range)
                      path 1 1)
           (expect range :to-equalp #())
           (expect size :to-be 3))))))
  (it "reads a range beginning at the first byte"
    (aitools.test.support:call-with-workspace
     (lambda (root state)
       (declare (ignore state))
       (let ((path (format nil "~A/data" root)))
         (aitools.test.support:write-bytes path #(1 2 3))
         (multiple-value-bind (range size)
             (funcall (cl-boundary-kit:boundary-context-require
                       (aitools.text.infrastructure:make-host-text-source)
                       :text-read-range)
                      path 0 2)
           (expect range :to-equalp #(1 2))
           (expect size :to-be 3))))))
  (it "clamps a range at end of file"
    (aitools.test.support:call-with-workspace
     (lambda (root state)
       (declare (ignore state))
       (let ((path (format nil "~A/data" root)))
         (aitools.test.support:write-bytes path #(1 2 3))
         (multiple-value-bind (range size)
             (funcall (cl-boundary-kit:boundary-context-require
                       (aitools.text.infrastructure:make-host-text-source)
                       :text-read-range)
                      path 1 99)
           (expect range :to-equalp #(2 3))
           (expect size :to-be 3))))))
  (it "reads a bounded range across a buffer boundary"
    (aitools.test.support:call-with-workspace
     (lambda (root state)
       (declare (ignore state))
       (let* ((path (format nil "~A/data" root))
              (bytes (make-array (* 3 65536) :initial-element 0)))
         (loop for offset from 65534 below 65539
               for value from 1
               do (setf (aref bytes offset) value))
         (aitools.test.support:write-bytes path bytes)
         (multiple-value-bind (range size)
             (funcall (cl-boundary-kit:boundary-context-require
                       (aitools.text.infrastructure:make-host-text-source)
                       :text-read-range)
                      path 65534 65539)
           (expect range :to-equalp #(1 2 3 4 5))
           (expect size :to-be (* 3 65536))))))))

