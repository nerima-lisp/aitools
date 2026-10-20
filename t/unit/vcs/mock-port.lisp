(in-package #:aitools.vcs.test)

(defvar *mock-port-functions* (make-hash-table :test (function eq)))

(defun respond-to-mock (response on-success on-failure)
  (if (and (consp response) (eq (first response) :fail))
      (funcall on-failure (second response) (third response))
      (funcall on-success response)))

(defun mock-port (&key (probe :repository) runs
                       (status nil status-supplied-p)
                       (numstat nil numstat-supplied-p)
                       (top "/repo") (directory "/repo"))
  (let ((port nil))
    (let* ((probe-mock
             (cl-weave:make-mock-function
              (lambda (&key on-repository on-outside on-missing on-no-directory on-failure)
                (ecase probe
                  (:repository (funcall on-repository top directory))
                  (:outside (funcall on-outside))
                  (:missing (funcall on-missing))
                  (:failure (funcall on-failure :failed "git probe failed"))
                  (:no-directory (funcall on-no-directory "/nowhere/"))))))
           (at-mock (cl-weave:make-mock-function (lambda (root) (declare (ignore root)) port)))
           (run-mock
             (cl-weave:make-mock-function
              (lambda (subcommand arguments &key octets on-success on-failure)
                (declare (ignore arguments octets))
                (let ((entry (assoc subcommand runs :test (function string=))))
                  (if entry
                      (let ((response (cdr entry)))
                        (respond-to-mock
                         (if (eq (first response) :ok) (second response) response)
                         on-success on-failure))
                      (funcall on-failure :failed
                               (format nil "unexpected git subcommand: ~A" subcommand)))))))
           (status-mock
             (cl-weave:make-mock-function
              (lambda (&key on-success on-failure)
                (if status-supplied-p
                    (respond-to-mock status on-success on-failure)
                    (funcall on-failure :failed "unexpected git status call")))))
           (numstat-mock
             (cl-weave:make-mock-function
              (lambda (arguments &key on-success on-failure)
                (declare (ignore arguments))
                (if numstat-supplied-p
                    (respond-to-mock numstat on-success on-failure)
                    (funcall on-failure :failed "unexpected git numstat call"))))))
      (setf port (make-git-port :probe probe-mock :run run-mock :status status-mock
                                :numstat numstat-mock :at at-mock)
            (gethash port *mock-port-functions*)
            (list (cons :probe probe-mock) (cons :run run-mock)
                  (cons :status status-mock) (cons :numstat numstat-mock)
                  (cons :at at-mock)))
      port)))


(defun mock-port-call-history (port)
  (let ((mocks (gethash port *mock-port-functions*)))
    (loop for (slot . mock) in mocks
          when (member slot (list :run :numstat :at))
          append (mapcar
                  (lambda (arguments)
                    (ecase slot
                      (:run (cons (first arguments) (second arguments)))
                      (:numstat (cons "numstat" (first arguments)))
                      (:at (cons "at" (first arguments)))))
                  (cl-weave:mock-calls mock)))))

(defun dispose-mock-port (port)
  (let ((mocks (gethash port *mock-port-functions*)))
    (when mocks
      (remhash port *mock-port-functions*)
      (dolist (entry mocks)
        (cl-weave:dispose-mock (cdr entry)))))
  port)

(defun run-flow (flow &rest arguments)
  "(VALUES KIND FIELDS CALLS) from the COMMAND-RESULT FLOW."
  (let* ((port (first arguments))
         (mocks (gethash port *mock-port-functions*)))
    (unwind-protect
         (let* ((result
                  (aitools.protocol.application:call-with-command-result/k
                   (lambda (&rest continuations)
                     (apply flow (append arguments continuations)))))
                (calls (and mocks (mock-port-call-history port))))
           (values (aitools.protocol.application:command-result-kind result)
                   (aitools.protocol.application:command-result-fields result)
                   calls))
      (dispose-mock-port port))))

(defun field (fields name &optional default)
  (let ((pair (assoc name fields :test #'string=)))
    (if pair (cdr pair) default)))

(defun first-repair-command (fields)
  (getf (first (getf fields :repairs)) :command))

(defun log-output (&rest commits)
  "`git log -z` text for COMMITS, each (SHA SUBJECT)."
  (apply #'nul-join (loop for (sha subject) in commits
                          append (list sha "Ada" "1704132245" "+0900" subject))))

(defun blame-output (line-count)
  (with-output-to-string (out)
    (loop for n from 1 to line-count
          do (format out "~A ~D ~D~@[ 1~]~%" (make-string 40 :initial-element #\a) n n (= n 1))
             (when (= n 1)
               (format out "author Ada~%author-time 1704132245~%author-tz +0900~%filename a.txt~%"))
             (format out "~Cline ~D~%" #\Tab n))))
