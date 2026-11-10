(in-package #:aitools.process.infrastructure)

(defun %linux-pidfd-open (pid)
  (declare (ignorable pid))
  #+(or x86-64 arm64)
  (let ((fd (sb-alien:alien-funcall
             (sb-alien:extern-alien
              "syscall"
              (function sb-alien:long sb-alien:long sb-alien:long sb-alien:long))
             434 pid 0)))
    (if (minusp fd)
        (values nil (sb-alien:get-errno))
        (values fd nil)))
  #-(or x86-64 arm64)
  (values nil sb-posix:enosys))

(defun %linux-pidfd-send-signal (fd signal)
  (declare (ignorable fd signal))
  #+(or x86-64 arm64)
  (let ((result (sb-alien:alien-funcall
                 (sb-alien:extern-alien
                  "syscall"
                  (function sb-alien:long sb-alien:long sb-alien:long
                            sb-alien:long sb-alien:system-area-pointer sb-alien:long))
                 424 fd signal (sb-sys:int-sap 0) 0)))
    (if (minusp result)
        (values nil (sb-alien:get-errno))
        (values t nil)))
  #-(or x86-64 arm64)
  (values nil sb-posix:enosys))

