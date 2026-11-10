(in-package #:aitools.journal.application)

(defstruct (journal-context (:constructor make-journal-context (&key root lock-timeout))
                            (:copier nil))
  ;; The global `--root` and `--lock-timeout` texts as given, or NIL.
  (root nil :type (or null string) :read-only t)
  (lock-timeout nil :type (or null string) :read-only t))
