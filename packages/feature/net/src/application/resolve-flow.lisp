(in-package #:aitools.net.application)
(defun %resolve-timeout-seconds (text)
    (when (stringp text)
      (handler-case
          (let* ((milliseconds
                   (aitools.kernel.domain:duration-milliseconds
                    (aitools.kernel.domain:parse-duration text)))
                 (seconds (/ milliseconds 1000)))
            (when (and (plusp milliseconds)
                       (< seconds most-positive-double-float))
              seconds))
        (aitools.kernel.domain:invalid-duration-error ()
          nil))))

  (aitools.kernel.domain:define-flow
      (resolve/k ports host &key (timeout "30s") on-ok on-error)
    (let ((timeout-seconds (%resolve-timeout-seconds timeout)))
      (cond
        ((null timeout-seconds)
         (aitools.kernel.domain:flow-continue
          :on-error
          "argument.invalid"
          (format nil "Timeout must be a positive finite duration: ~S" timeout)))
        ((not (and (stringp host)
                   (plusp (length host))
                   (notany (lambda (character)
                             (zerop (char-code character)))
                           host)))
         (aitools.kernel.domain:flow-continue
          :on-error
          "argument.invalid"
          "Host must be a non-empty string without NUL."))
        (t
         (handler-case
             (let* ((addresses
                      (funcall (net-ports-resolve-host ports)
                               host
                               :timeout timeout-seconds))
                    (records
                      (mapcar (lambda (address)
                                (aitools.protocol.domain:json-object-from-alist
                                 (list (cons "name" host)
                                      (cons "type"
                                            (aitools.net.domain:net-address-type address))
                                      (cons "value"
                                            (aitools.net.domain:net-address-value address)))))
                              addresses)))
               (aitools.kernel.domain:flow-continue
                :on-ok
                (list (cons "records" records))))
           (aitools.net.domain:net-error (condition)
             (aitools.kernel.domain:flow-continue
              :on-error
              (aitools.net.domain:net-error-code condition)
              (aitools.net.domain:net-error-detail condition))))))))
