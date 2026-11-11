;;;;
;;;; The scan uses one cl-concurrent-kit executor, sized to the online CPU
;;;; count, when multi-file work needs parallelism. It is created lazily for
;;;; the first multi-item batch and kept for the scan's dynamic extent.
;;;; EXECUTOR-MAP returns results in input order, which keeps parallel output
;;;; in path order. Worker threads do not inherit the caller's dynamic
;;;; bindings, so per-file work must not rely on specials bound by the caller.
(in-package #:aitools.workspace.infrastructure)

(defun %sysconf-online-processors ()
  #+darwin
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
   58)
  #+linux
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
   84)
  #-(or darwin linux)
  0)

(defun processor-count ()
  "Online CPUs from sysconf(_SC_NPROCESSORS_ONLN), at least 1."
  (declare (notinline %sysconf-online-processors))
  (let ((count (%sysconf-online-processors)))
    (or (and (plusp count) count) 1)))

(defun call-with-ordered-mapper (thunk)
  "Call THUNK with an ordered mapper, creating a worker pool only on the
first multi-item call and keeping it alive through THUNK's dynamic extent."
  (let ((size nil)
        (executor nil))
    (labels ((map-inline (function items)
               (mapcar function items))
             (map-parallel (function items)
               (unless size
                 (setf size (processor-count)))
               (if (= size 1)
                   (map-inline function items)
                   (progn
                     (unless executor
                       (setf executor
                             (cl-concurrent-kit:make-executor
                              :size size :name "aitools scan")))
                     (cl-concurrent-kit:executor-map executor function items)))))
      (unwind-protect
           (funcall thunk
                    (lambda (function items)
                      (if (rest items)
                          (map-parallel function items)
                          (map-inline function items))))
        (when executor
          (cl-concurrent-kit:shutdown-executor executor :wait t))))))
