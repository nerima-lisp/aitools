;;;;
;;;; Correspondence table rows 38-39: running commands, background processes,
;;;; and waiting.
(in-package #:aitools.e2e.test)

(defun trimmed (text)
  (string-trim '(#\Newline #\Space) text))

(defun expect-child-process-span (envelope name outcome)
  (let* ((spans (jlist (jget envelope "meta" "trace" "spans")))
         (command (find-if (lambda (span)
                             (and (string= (jget span "kind") "command")
                                  (null (jget span "parent_span_id"))))
                           spans))
         (child (find name spans :key (lambda (span) (jget span "name")) :test #'string=)))
    (expect command :to-be-truthy)
    (expect child :to-be-truthy)
    (multiple-value-bind (parent present-p) (gethash "parent_span_id" command)
      (expect present-p :to-be t)
      (expect parent :to-be nil))
    (expect (jget child "kind") :to-equal "child_process")
    (expect (jget child "outcome") :to-equal outcome)
    (expect (jget child "parent_span_id") :to-be (jget command "span_id"))))

(describe "aitools child-process trace spans (e2e)"
  (it "records process.run with outcomes and the command parent"
    (:tags (quote (:trace-positive)))
    (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
            (and directory (plusp (length directory))))
      (cl-weave:skip "trace specs require a binary with --trace"))
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "occupied.out" "occupied")
       (let* ((success (aitools ws '("--trace" "run" "--" "sh" "-c" "printf child")
                               :expect-trace t))
              (stdin (aitools ws '("--trace" "run" "--stdin" "--" "cat")
                              :stdin "stream input"
                              :expect-trace t))
              (timeout (aitools ws '("--trace" "run" "--stdin" "--timeout" "100ms" "--" "sleep" "2")
                                :stdin ""
                                :expect-trace t))
              (unavailable (aitools ws '("--trace" "run" "--" "aitools-no-such-program-fr014")
                                    :expect-trace t))
              (error (aitools ws '("--trace" "run" "--stdout-to" "occupied.out" "--" "sh" "-c" "printf overwrite")
                              :expect-trace t)))
         (expect (aitools-exit-code success) :to-be 0)
         (expect (jget (aitools-envelope success) "stdout" "head" 0) :to-equal "child")
         (expect-child-process-span (aitools-envelope success) "process.run" "ok")
         (expect (aitools-exit-code stdin) :to-be 0)
         (expect (jget (aitools-envelope stdin) "stdout" "head" 0) :to-equal "stream input")
         (expect-child-process-span (aitools-envelope stdin) "process.run" "ok")
         (expect (aitools-exit-code timeout) :to-be 0)
         (expect (jget (aitools-envelope timeout) "timed_out") :to-be t)
         (expect-child-process-span (aitools-envelope timeout) "process.run" "timeout")
         (expect-child-process-span (aitools-envelope unavailable) "process.run" "unavailable")
         (expect-child-process-span (aitools-envelope error) "process.run" "error")))))

  (it "records process.launch-detached and stops the started child"
    (:tags (quote (:trace-positive)))
    (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
            (and directory (plusp (length directory))))
      (cl-weave:skip "trace specs require a binary with --trace"))
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let* ((started (aitools ws '("--trace" "bg" "start" "--" "sh" "-c" "sleep 30")
                                 :expect-trace t))
              (id (jget (aitools-envelope started) "id")))
         (unwind-protect
             (progn
               (expect (aitools-exit-code started) :to-be 0)
               (expect id :to-be-truthy)
               (expect-child-process-span (aitools-envelope started)
                                          "process.launch-detached" "ok"))
           (when id
             (let ((stopped (aitools ws (list "--trace" "bg" "stop" id "--grace" "1s")
                                     :expect-trace t)))
               (expect (aitools-exit-code stopped) :to-be 0)))))))))

(define-row-case (38 "cmd | grep is run --grep" :foreign ("grep")) (ws)
  (expect (format nil "~{~A~%~}"
                  (mapcar (lambda (match) (jget match "text"))
                          (jlist (jget (run-ok ws '("run" "--grep" "1" "--" "seq" "1" "12")) "stdout" "matches"))))
          :to-equal (oracle ws '("seq" "grep") "seq 1 12 | grep 1" (format nil "1~%10~%11~%12~%"))))

(define-row-case (38 "cmd | head -n 2 is run --head 2 --tail 0" :foreign ("head")) (ws)
  (expect (lines-text (jget (run-ok ws '("run" "--head" "2" "--tail" "0" "--" "seq" "1" "12")) "stdout" "head"))
          :to-equal (oracle ws '("seq" "head") "seq 1 12 | head -n 2" (format nil "1~%2~%"))))

(define-row-case (38 "cmd | tail -n 2 is run --head 0 --tail 2" :foreign ("tail")) (ws)
  (expect (lines-text (jget (run-ok ws '("run" "--head" "0" "--tail" "2" "--" "seq" "1" "12")) "stdout" "tail"))
          :to-equal (oracle ws '("seq" "tail") "seq 1 12 | tail -n 2" (format nil "11~%12~%"))))

(define-row-case (38 "cmd > file is run --stdout-to" :foreign ()) (ws)
  (let ((expected (oracle ws '("seq") "seq 1 3 > o.txt && cat o.txt" (format nil "1~%2~%3~%"))))
    (run-ok ws '("run" "--stdout-to" "a.txt" "--" "seq" "1" "3"))
    (expect (file-text ws "a.txt") :to-equal expected)))

(define-row-case (38 "stripping ANSI colors with perl is run's default" :foreign ("perl")) (ws)
  (let ((script "printf '\\033[1;31mred\\033[0m plain\\n'"))
    (expect (lines-text (jget (run-ok ws (list "run" "--" "sh" "-c" script)) "stdout" "head"))
            :to-equal (oracle ws '("perl") (format nil "~A | perl -pe 's/\\e\\[[0-9;]*m//g'" script)
                              (format nil "red plain~%")))))

(defmacro with-bg ((id-var workspace argv) &body body)
  "Start ARGV with bg start, bind its id, and stop it on the way out."
  (let ((ws (gensym "WS")))
    `(let* ((,ws ,workspace)
            (,id-var (jget (run-ok ,ws (list* "bg" "start" "--" ,argv)) "id")))
       (unwind-protect (progn ,@body)
         (aitools ,ws (list "bg" "stop" ,id-var "--grace" "1s"))))))

(define-row-case (39 "background command output is available through bg start, wait --bg, and bg logs" :foreign ("sleep")) (ws)
  (let ((script "echo one; echo two; sleep 30"))
    (with-bg (id ws (list "sh" "-c" script))
      (expect (jget (run-ok ws (list "wait" "--bg" id "--pattern" "two" "--timeout" "10s")) "matched") :to-be t)
      (expect (lines-text (jget (run-ok ws (list "bg" "logs" id)) "lines"))
              :to-equal (oracle ws '() "echo one; echo two" (format nil "one~%two~%"))))))

(define-row-case (39 "tail -c +N of a log is bg logs --from" :foreign ("tail")) (ws)
  (with-bg (id ws (list "sh" "-c" "echo one; echo two; sleep 30"))
    (run-ok ws (list "wait" "--bg" id "--pattern" "two" "--timeout" "10s"))
    (expect (lines-text (jget (run-ok ws (list "bg" "logs" id "--from" "4")) "lines"))
            :to-equal (oracle ws '("tail") "printf 'one\\ntwo\\n' | tail -c +5" (format nil "two~%")))))

(define-row-case (39 "sleep is wait --duration" :foreign ("sleep")) (ws)
  (let ((envelope (run-ok ws '("wait" "--duration" "300ms"))))
    (expect (jget envelope "matched") :to-be t)
    (expect (>= (jget envelope "elapsed_ms") 300) :to-be t)))

(define-row-case (39 "waiting for a port as nc -z sees it is wait --port" :foreign ("nc")) (ws)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen socket 5)
           (let ((port (nth-value 1 (sb-bsd-sockets:socket-name socket))))
             (expect (trimmed (oracle ws '("nc") (format nil "nc -z 127.0.0.1 ~D; echo $?" port) (format nil "0~%")))
                     :to-equal "0")
             (expect (jget (run-ok ws (list "wait" "--port" (princ-to-string port) "--timeout" "5s")) "matched")
                     :to-be t)))
      (sb-bsd-sockets:socket-close socket))))

(define-row-case (39 "timeout's 124 is run --timeout's timed_out" :foreign ("timeout")) (ws)
  (expect (if (jget (run-ok ws '("run" "--timeout" "1s" "--" "sleep" "5")) "timed_out") "124" "0")
          :to-equal (trimmed (oracle ws '("timeout") "timeout 1 sleep 5; echo $?" (format nil "124~%")))))

