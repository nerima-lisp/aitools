(in-package #:aitools.env.presentation)

(defun %run-flow (flow continuation-keys &rest arguments)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow
            (append arguments
                    (loop for key in continuation-keys
                          append (list key (getf continuations key))))))))

(defun %schema-data (full-name)
  (or (find full-name aitools.data:*env-command-schemas*
            :key (lambda (entry) (getf entry :name)) :test #'string=)
      (error "no env schema data for ~S" full-name)))

(defun %schema (full-name)
  (let ((data (%schema-data full-name)))
    (aitools.protocol.domain:make-command-schema
     full-name
     (getf data :summary)
     :description (getf data :description)
     :args (getf data :args)
     :output-fields (getf data :output-fields)
     :error-codes (getf data :error-codes))))

(defun %tz-option ()
  (make-option :name "tz" :kind :value :value-name "IANA-NAME"
               :description "IANA time zone name, e.g. Asia/Tokyo (default: $TZ, then /etc/localtime)."))
