;;;; packages/feature/vcs/src/domain/render.lisp
;;;;
;;;; JSON value helpers and the command-line text used in `next_commands`.
;;;; json-kit writes NIL as `[]`, so absent scalars and false flags need its
;;;; explicit null and false values.
(in-package #:aitools.vcs.domain)

(defun command-line (&rest words)
  "Join WORDS (strings; NIL entries are dropped) into one shell command line,
quoting each word with aitools.protocol.domain:shell-quote."
  (aitools.protocol.domain:command-line (remove nil words)))

(defun %parse-offset-minutes (offset)
  "Minutes east of UTC for a git `+HHMM`/`-HHMM` offset string."
  (unless (and (= (length offset) 5) (find (char offset 0) "+-")
               (every (lambda (char) (char<= #\0 char #\9)) (subseq offset 1)))
    (error "malformed timezone offset: ~S" offset))
  (let ((minutes (+ (* 60 (parse-integer offset :start 1 :end 3))
                    (parse-integer offset :start 3 :end 5))))
    (if (char= (char offset 0) #\-) (- minutes) minutes)))

(defun epoch-seconds-to-iso8601 (seconds offset)
  "Render Unix time SECONDS in the zone of git offset string OFFSET
(`+0900`) as `YYYY-MM-DDTHH:MM:SS+09:00`. UTC renders as `+00:00` on every
git version, unlike `%aI`, which newer git prints as `Z`."
  (let ((minutes (%parse-offset-minutes offset)))
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time (+ seconds (* 60 minutes) (encode-universal-time 0 0 0 1 1 1970 0)) 0)
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~A~2,'0D:~2,'0D"
              year month day hour minute second
              (if (minusp minutes) "-" "+")
              (floor (abs minutes) 60) (mod (abs minutes) 60)))))
