(in-package #:aitools.process.domain)

(defstruct (process-identity (:constructor make-process-identity
                               (&key pid ppid pgid uid ruid start command-line)))
  pid ppid pgid uid ruid start command-line)

(defun same-process-p (expected actual)
  (and expected actual
       (= (process-identity-pid expected) (process-identity-pid actual))
       (= (process-identity-uid expected) (process-identity-uid actual))
       (= (process-identity-ruid expected) (process-identity-ruid actual))
       (equal (process-identity-start expected) (process-identity-start actual))
       (string= (process-identity-command-line expected)
                (process-identity-command-line actual))
       (= (process-identity-ppid expected) (process-identity-ppid actual))
       (= (process-identity-pgid expected) (process-identity-pgid actual))))

(defun signal-number (name)
  (cdr (assoc (and (stringp name) (string-upcase name))
              aitools.data:*process-signal-numbers*
              :test #'string=)))
