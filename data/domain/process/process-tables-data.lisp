(in-package #:aitools.data)

(defparameter *process-signal-numbers*
  '(("TERM" . 15) ("KILL" . 9) ("HUP" . 1) ("INT" . 2)
    ("QUIT" . 3)
    #+darwin ("USR1" . 30) #+darwin ("USR2" . 31)
    #+linux ("USR1" . 10) #+linux ("USR2" . 12)))

(defparameter *process-bg-file-suffixes* '(".json" ".log" ".exit"))

(defparameter *process-bg-record-keys*
  '("id" "name" "argv" "pid" "started" "stop_signal"))

(export '(*process-signal-numbers*
          *process-bg-file-suffixes*
          *process-bg-record-keys*))
