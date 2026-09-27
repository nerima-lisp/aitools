;;;; packages/core/workspace/src/infrastructure/ordered-mapper.lisp
;;;;
;;;; The scan's worker pool: one cl-concurrent-kit executor sized to the
;;;; online CPU count for the duration of one scan. EXECUTOR-MAP returns
;;;; results in input order, which is what keeps parallel output in path
;;;; order. Worker threads do not inherit the caller's dynamic bindings, so
;;;; per-file work must not rely on specials bound by the caller.
(in-package #:aitools.workspace.infrastructure)

(defun processor-count ()
  "Online CPUs from sysconf(_SC_NPROCESSORS_ONLN), at least 1."
  (let ((name #+darwin 58 #+linux 84 #-(or darwin linux) nil))
    (or (and name
             (let ((count (sb-alien:alien-funcall
                           (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
                           name)))
               (and (plusp count) count)))
        1)))

(defun call-with-ordered-mapper (thunk)
  "Call THUNK with a mapper (function items) -> results in ITEMS order,
running FUNCTION on a pool of PROCESSOR-COUNT threads that lives only for
THUNK's dynamic extent."
  (let ((size (processor-count)))
    (if (= size 1)
        (funcall thunk (lambda (function items) (mapcar function items)))
        (cl-concurrent-kit:with-executor (executor :size size :name "aitools scan")
          (funcall thunk (lambda (function items)
                           (if (rest items)
                               (cl-concurrent-kit:executor-map executor function items)
                               (mapcar function items))))))))
