(in-package #:aitools.net.application)
(aitools.kernel.domain:define-flow
 (resolve/k boundaries host &key (timeout "30s") on-ok on-error)
 (declare (type function on-ok on-error))
 (let ((timeout-seconds (%net-timeout-seconds timeout)))
   (%with-net-validation
       ((timeout-seconds
         (format nil "Timeout must be a positive finite duration: ~S" timeout))
        ((and (stringp host)
              (plusp (length host))
              (notany (lambda (character)
                        (zerop (char-code character)))
                      host))
         "Host must be a non-empty string without NUL."))
     (%with-net-error-continuation ()
       (let* ((addresses
                (funcall
                 (cl-boundary-kit:boundary-context-require
                  boundaries :net-resolve)
                 host :timeout timeout-seconds))
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
          :on-ok (list (cons "records" records))))))))
