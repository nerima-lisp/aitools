(in-package #:aitools.env.infrastructure)

#+darwin
(defun %sysctl-mib (name)
  (let* ((boot-time-p (eq name :kern-boottime))
         (expected-size (if boot-time-p 16 24))
         (bytes (make-array expected-size
                            :element-type (quote (unsigned-byte 8))
                            :initial-element 0)))
    ;; CTL_KERN/KERN_BOOTTIME = {1, 21}; CTL_VM/VM_LOADAVG = {2, 2}.
    (sb-sys:with-pinned-objects (bytes)
      (sb-alien:with-alien ((mib (array sb-alien:int 2))
                            (actual-size sb-alien:unsigned-long))
        (setf (sb-alien:deref mib 0) (if boot-time-p 1 2)
              (sb-alien:deref mib 1) (if boot-time-p 21 2)
              actual-size expected-size)
        (let ((sysctl
                (sb-alien:extern-alien
                 "sysctl"
                 (function sb-alien:int
                           (* (array sb-alien:int 2))
                           sb-alien:unsigned-int
                           (* sb-alien:unsigned-char)
                           (* sb-alien:unsigned-long)
                           (* sb-alien:unsigned-char)
                           sb-alien:unsigned-long))))
          (when (and (zerop
                      (sb-alien:alien-funcall
                       sysctl
                       (sb-alien:addr mib)
                       2
                       (sb-alien:sap-alien
                        (sb-sys:vector-sap bytes)
                        (* sb-alien:unsigned-char))
                       (sb-alien:addr actual-size)
                       (sb-alien:sap-alien
                        (sb-sys:int-sap 0)
                        (* sb-alien:unsigned-char))
                       0))
                     (= actual-size expected-size))
            (let ((sap (sb-sys:vector-sap bytes)))
              (if boot-time-p
                  (let ((seconds (sb-sys:signed-sap-ref-64 sap 0))
                        (microseconds (sb-sys:signed-sap-ref-32 sap 8)))
                    (when (and (>= seconds 0)
                               (<= 0 microseconds)
                               (< microseconds 1000000))
                      (+ seconds (/ microseconds 1d6))))
                  (let ((scale (sb-sys:signed-sap-ref-64 sap 16)))
                    (when (plusp scale)
                      (mapcar (lambda (offset)
                                (/ (float (sb-sys:sap-ref-32 sap offset) 1d0)
                                   scale))
                              (list 0 4 8))))))))))))
