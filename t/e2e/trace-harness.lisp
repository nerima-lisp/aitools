(in-package #:aitools.e2e.test)

(defun aitools (workspace arguments &key stdin env (directory "") expect-trace)
  "Run the executable and require one version 2 JSON envelope."
  (let* ((binary (aitools-binary))
         (result (run-process (native binary) arguments
                              :directory (ws-path workspace directory)
                              :environment (workspace-environment workspace env)
                              :input (and stdin (if (stringp stdin) (octets stdin) stdin))))
         (stdout (utf8 (result-stdout result)))
         (stderr (utf8 (result-stderr result)))
         (text (cond ((and (plusp (length stdout)) (zerop (length stderr))) stdout)
                     ((and (zerop (length stdout)) (plusp (length stderr))) stderr)
                     (t (fail "~A" (format nil "aitools ~{~A~^ ~}: expected one JSON envelope on exactly one stream, got stdout ~S and stderr ~S"
                                          arguments stdout stderr)))))
         (envelope (handler-case (json-kit:parse text :false-value nil :null-value nil)
                     (json-kit:json-kit-error (condition)
                       (fail "~A" (format nil "aitools ~{~A~^ ~}: output is not JSON (~A): ~S" arguments condition text))))))
    (unless (eql (jget envelope "schema_version") 2)
      (fail "aitools ~{~A~^ ~}: expected schema_version 2, got ~S"
            arguments (jget envelope "schema_version")))
    (if expect-trace
        (let ((meta (jget envelope "meta")))
          (unless (and (hash-table-p meta) (nth-value 1 (gethash "trace" meta)))
            (fail "aitools ~{~A~^ ~}: expected meta.trace with trace instrumentation" arguments)))
        (progn
          (when (nth-value 1 (gethash "meta" envelope))
            (fail "aitools ~{~A~^ ~}: expected meta to be absent without trace instrumentation" arguments))
          (capture-no-trace-output
           aitools/test::*current-spec-id*
           (incf aitools/test::*current-spec-no-trace-invocation*)
           result)))
    (make-aitools-result
     :exit-code (result-exit-code result)
     :envelope envelope
     :stream (if (eq text stdout) :stdout :stderr)
     :stdout stdout :stderr stderr :argv arguments)))
