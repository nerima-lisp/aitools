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
              '(("TERM" . 15) ("KILL" . 9) ("HUP" . 1) ("INT" . 2)
                ("QUIT" . 3)
                #+darwin ("USR1" . 30) #+darwin ("USR2" . 31)
                #+linux ("USR1" . 10) #+linux ("USR2" . 12))
              :test #'string=)))
