(in-package #:aitools.util.application)

(aitools.kernel.domain:define-flow
    (util-uuid-flow ports kind count &key on-ok on-error)
  "KIND is \"v4\" or \"v7\". v7 values from one call are strictly increasing."
  (declare (type function on-ok on-error))
  (cond
    ((not (<= 1 count +util-max-count+))
     (aitools.kernel.domain:flow-call (%count-error "util uuid")
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))
    ((string= kind "v4")
     (aitools.kernel.domain:flow-continue
      :on-ok
      (list
       (cons "values"
             (loop repeat count
                   collect (funcall (util-ports-uuid-v4 ports)))))))
    ((string= kind "v7")
     (let ((state (aitools.util.domain:make-uuid-v7-state)))
       (aitools.kernel.domain:flow-continue
        :on-ok
        (list
         (cons "values"
               (loop repeat count
                     collect
                     (aitools.util.domain:next-uuid-v7
                      state
                      (funcall (util-ports-unix-ms ports))
                      (funcall (util-ports-random-octets ports)
                               aitools.util.domain:+uuid-v7-random-octets+))))))))
    (t
     (aitools.kernel.domain:flow-call
         (%choice-error "--kind" kind +util-uuid-kinds+ "util uuid")
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))))

(defun %octet-supplier (ports)
  "A closure returning one random octet per call, drawing from the
RANDOM-OCTETS port in blocks so the port is not called per character."
  (let ((buffer (funcall (util-ports-random-octets ports) 64)) (index 0))
    (lambda ()
      (when (= index (length buffer))
        (setf buffer (funcall (util-ports-random-octets ports) 64) index 0))
      (prog1 (aref buffer index) (incf index)))))

(aitools.kernel.domain:define-flow
    (util-random-flow ports length alphabet count &key on-ok on-error)
  (declare (type function on-ok on-error))
  (cond
    ((not (<= 1 count +util-max-count+))
     (aitools.kernel.domain:flow-call (%count-error "util random")
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))
    ((not (<= 1 length +util-max-random-length+))
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid"
      (format nil "--length must be between 1 and ~D" +util-max-random-length+)
      :repairs
      (list
       (repair
        "use-default" "Use the default length."
        "aitools util random --length 32"))))
    ((not (aitools.util.domain:random-alphabet-p alphabet))
     (aitools.kernel.domain:flow-call
         (%choice-error "--alphabet" alphabet +util-random-alphabets+ "util random")
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))
    (t
     (let ((next-octet (%octet-supplier ports)))
       (aitools.kernel.domain:flow-continue
        :on-ok
        (list
         (cons "values"
               (loop repeat count
                     collect
                     (aitools.util.domain:random-alphabet-string
                      alphabet length next-octet)))))))))
