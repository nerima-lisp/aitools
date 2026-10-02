;;;; t/support/cli.lisp
(in-package #:aitools.test.support)

(defun envelope-value (envelope &rest keys)
  "Traverse an envelope by string member names and non-negative vector indexes."
  (reduce (lambda (value key)
            (cond ((null value) nil)
                  ((integerp key)
                   (and (vectorp value) (< -1 key) (< key (length value))
                        (aref value key)))
                  ((hash-table-p value) (gethash key value))
                  (t nil)))
          keys :initial-value envelope))

(defun run-aitools (root arguments &key stdin)
  "Dispatch ARGUMENTS against ROOT and return CODE, parsed ENVELOPE, STREAM.
STREAM is :STDOUT or :STDERR, selected from the one stream containing output."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (argv (list* "aitools" "--root" root arguments))
           (code (with-input-from-string (*standard-input* (or stdin ""))
                   (aitools/cli:dispatch app registry argv :stdout out :stderr err)))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err)))
      (when (and (plusp (length stdout)) (plusp (length stderr)))
        (error "aitools wrote to both stdout and stderr"))
      (let ((stream (if (plusp (length stdout)) :stdout :stderr))
            (text (if (plusp (length stdout)) stdout stderr)))
        (values code (json-kit:parse text) stream)))))
