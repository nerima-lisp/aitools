;;;; packages/feature/inspect/src/domain/toml-values.lisp
(in-package #:aitools.inspect.domain)

(define-condition toml-unsupported-value (error)
  ((reason :initarg :reason :reader toml-unsupported-reason)
   (pointer :initarg :pointer :reader toml-unsupported-pointer)))
(define-condition toml-syntax-error (error) ())

(defun %toml-unsupported (reason tokens)
  (error 'toml-unsupported-value
         :reason reason
         :pointer (format-json-pointer tokens)))

(defun %toml-trim-fraction (text)
  (let ((dot (position #\. text)))
    (if (null dot)
        text
        (let* ((suffix (or (position-if (lambda (char) (find char "Zz+-"))
                                         text
                                         :start (1+ dot))
                           (length text)))
               (end suffix))
          (loop while (and (> end (1+ dot)) (char= (char text (1- end)) #\0))
                do (decf end))
          (concatenate 'string
                       (subseq text 0 (if (= end (1+ dot)) dot end))
                       (subseq text suffix))))))

(defun %toml-normalize-date-time (text)
  (let* ((canonical (substitute #\T #\t text))
         (suffix (position-if (lambda (char) (find char "+-"))
                              canonical :start (min 19 (length canonical)))))
    (when (char= (char canonical (1- (length canonical))) #\z)
      (setf (char canonical (1- (length canonical))) #\Z))
    (if (and suffix
             (member (subseq canonical suffix) '("+00:00" "-00:00")
                     :test #'string=))
        (concatenate 'string (subseq canonical 0 suffix) "Z")
        canonical)))

(defun %toml-date-string (value formatter &key offset)
  (let ((text (funcall formatter value)))
    (setf text (%toml-trim-fraction text))
    (if offset (%toml-normalize-date-time text) text)))

(defun %toml-to-json (value tokens)
  (cond
    ((cl-toml-kit:toml-false-p value) (json-false))
    ((eq value t) t)
    ((stringp value) value)
    ((integerp value) value)
    ((floatp value)
     (handler-case
         (if (and (= value value)
                  (<= (- most-positive-double-float) value most-positive-double-float))
             value
             (%toml-unsupported "non-finite-float" tokens))
       (floating-point-invalid-operation ()
         (%toml-unsupported "non-finite-float" tokens))))
    ((cl-date-kit:offset-date-time-p value)
     (%toml-date-string value #'cl-date-kit:format-offset-date-time :offset t))
    ((cl-date-kit:local-date-time-p value)
     (%toml-date-string value #'cl-date-kit:format-local-date-time))
    ((cl-date-kit:local-date-p value)
     (%toml-date-string value #'cl-date-kit:format-local-date))
    ((cl-date-kit:local-time-p value)
     (%toml-date-string value #'cl-date-kit:format-local-time))
    ((hash-table-p value)
     (let ((pairs nil))
       (maphash
        (lambda (key item)
          (unless (stringp key)
            (%toml-unsupported "non-string-key" tokens))
          (push (cons key (%toml-to-json item (append tokens (list key)))) pairs))
        value)
       (json-object-from-pairs (nreverse pairs))))
    ((and (vectorp value) (not (stringp value)))
     (coerce
      (loop for item across value for index from 0
            collect (%toml-to-json
                     item (append tokens (list (princ-to-string index)))))
      'vector))
    (t (error "unexpected TOML parser value: ~S" value))))

(defun parse-toml-document (text)
  (handler-case
      (%toml-to-json (cl-toml-kit:parse text) nil)
    (cl-toml-kit:toml-parse-error ()
      (error 'toml-syntax-error))))

(defun %toml-write-check-value (value tokens depth max-depth)
  (let* ((container-p
           (or (hash-table-p value)
               (and (vectorp value) (not (stringp value)))))
         (current-depth (if container-p (1+ depth) depth)))
    (when (and container-p max-depth (> current-depth max-depth))
      (error 'toml-syntax-error))
    (cond
      ((cl-date-kit:offset-date-time-p value)
       (%toml-unsupported "offset-date-time" tokens))
      ((cl-date-kit:local-date-time-p value)
       (%toml-unsupported "local-date-time" tokens))
      ((cl-date-kit:local-date-p value)
       (%toml-unsupported "local-date" tokens))
      ((cl-date-kit:local-time-p value)
       (%toml-unsupported "local-time" tokens))
      ((floatp value)
       (unless (handler-case
                   (and (= value value)
                        (<= (- most-positive-double-float)
                            value
                            most-positive-double-float))
                 (floating-point-invalid-operation () nil))
         (%toml-unsupported "non-finite-float" tokens)))
      ((hash-table-p value)
       (maphash (lambda (key item)
                  (%toml-write-check-value
                   item (append tokens (list key)) current-depth max-depth))
                value))
      ((and (vectorp value) (not (stringp value)))
       (loop for item across value
             for index from 0
             do (%toml-write-check-value
                 item (append tokens (list (princ-to-string index)))
                 current-depth max-depth))))))

(defun parse-toml-write-document (text &key max-depth)
  (handler-case
      (let ((native (cl-toml-kit:parse text)))
        (%toml-write-check-value native nil 0 max-depth)
        (values (%toml-to-json native nil)
                (length (cl-toml-kit:toml-comment-spans text))))
    (cl-toml-kit:toml-parse-error ()
      (error 'toml-syntax-error))))
