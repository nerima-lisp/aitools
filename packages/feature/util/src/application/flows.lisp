;;;; packages/feature/util/src/application/flows.lisp
;;;;
;;;; One flow per util command. Every flow calls exactly
;;;; one of ON-OK (an alist of output fields, in output order) or ON-ERROR
;;;; (code message &key repairs), per the command handler contract of
;;;; AITOOLS.PROTOCOL.APPLICATION:CALL-WITH-COMMAND-RESULT/K.
(in-package #:aitools.util.application)

(defparameter +util-codec-schemes+ aitools.util.domain:+codec-schemes+)

(defparameter +util-random-alphabets+ (mapcar #'car aitools.util.domain:+random-alphabets+)
  "Alphabet names; the first is the `--alphabet` default.")

(defparameter +util-uuid-kinds+ '("v4" "v7"))

(defconstant +util-default-decimals+ 10)

(defconstant +util-max-count+ 1000
  "Largest `--count` for `util uuid` and `util random`.")

(defconstant +util-max-random-length+ 4096
  "Largest `--length` for `util random`.")

(defun %schema-repair (command)
  (aitools.protocol.domain:schema-repair (format nil "aitools schema ~A" command)))

(defun %input-text (octets text)
  (or text (aitools.util.domain:octets->lenient-text octets)))

(defun %choice-error (on-error option value choices command)
  (funcall on-error "argument.invalid" (format nil "~A must be one of ~{~A~^, ~}, not ~S" option choices value)
           :repairs (list (%schema-repair command))))

(defun util-encode-flow (ports scheme request &key on-ok on-error)
  (if (not (aitools.util.domain:codec-scheme-p scheme))
      (%choice-error on-error "scheme" scheme +util-codec-schemes+ "util encode")
    (resolve-input/k ports request (format nil "util encode ~A" scheme)
                     :on-input (lambda (octets &key text)
                                 (declare (ignore text))
                                 (funcall on-ok (list (cons "output" (aitools.util.domain:encode-octets scheme octets)))))
                     :on-error on-error)))

(defun util-decode-flow (ports scheme request &key to root lock-timeout dry-run tx display-argv on-ok on-error)
  "Decode to `output` text when the result is valid UTF-8, otherwise report
`binary:true` with the bytes as `output_hex`. The text case carries
no `binary` member: the flag exists only for binary output.

With TO, the decoded bytes are written to that path instead, through the
edit context's write pipeline (journaled, undoable; bytes as they
are, exempt from the text-encoding rules), and the answer is the standard write output plus
`bytes`. ROOT and LOCK-TIMEOUT are the global options; DRY-RUN and TX apply
only with TO."
  (cond
    ((not (aitools.util.domain:codec-scheme-p scheme))
     (%choice-error on-error "scheme" scheme +util-codec-schemes+ "util decode"))
    ((and (null to) (or dry-run tx))
     (funcall on-error "argument.invalid" "--dry-run and --tx apply only to a write: add --to <path>"
              :repairs (list (%schema-repair "util decode"))))
    (t
     (resolve-input/k
      ports request (format nil "util decode ~A" scheme)
      :on-input
      (lambda (octets &key text)
        (declare (ignore text))
        (aitools.util.domain:decode-text/k
         scheme octets
         :on-decoded
         (lambda (decoded)
           (if to
               (%write-decoded ports scheme octets decoded to
                               :root root :lock-timeout lock-timeout :dry-run dry-run :tx tx
                               :display-argv display-argv :on-ok on-ok :on-error on-error)
               (aitools.util.domain:octets->utf-8/k
                decoded
                :on-text (lambda (output)
                           (funcall on-ok (list (cons "bytes" (length decoded)) (cons "output" output))))
                :on-invalid (lambda (offset)
                              (declare (ignore offset))
                              (funcall on-ok (list (cons "binary" t) (cons "bytes" (length decoded))
                                                   (cons "output_hex" (aitools.util.domain:octets->hex decoded))))))))
         :on-invalid
         (lambda (offset reason)
           (funcall on-error "input.syntax-error" (format nil "invalid ~A input at byte ~D: ~A" scheme offset reason)
                    :repairs (list (%schema-repair "util decode"))))))
      :on-error on-error))))

(defun %write-decoded (ports scheme input decoded to &key root lock-timeout dry-run tx display-argv on-ok on-error)
  "Write DECODED to TO as a new file; an existing TO is refusal.exists."
  (aitools.edit.application:run-write-command/k
   (aitools.edit.application:make-write-edit-ports :workspace-host (util-ports-workspace-host ports)
                                                   :open-store (util-ports-open-store ports))
   (aitools.edit.application:make-write-plan
    :command "util.decode"
    :targets (list (aitools.edit.application:make-write-target to))
    :inputs (list input decoded)
    :record-options (lambda (paths) (list :to (first paths)))
    :record-positionals (lambda (paths) (declare (ignore paths)) (list scheme))
    :plan (lambda (context commit reject)
            (let ((path (aitools.edit.application:context-path context)))
              (if (aitools.store.domain:entry-state-absent-p
                   (aitools.store.application:view-path-state (aitools.edit.application:write-context-view context) path))
                  (funcall commit (list (aitools.store.domain:write-file-request path decoded))
                           (list (cons "bytes" (length decoded))))
                  (funcall reject "refusal.exists"
                           (format nil "~A already exists; util decode --to writes only a new file" path))))))
   :root root :lock-timeout lock-timeout :dry-run dry-run :tx tx :display-argv display-argv
   :on-ok on-ok :on-error on-error))

(defun util-redact-flow (ports request &key on-ok on-error)
  (resolve-input/k ports request "util redact"
                   :on-input (lambda (octets &key text)
                               (multiple-value-bind (redacted count)
                                   (aitools.protocol.domain:redact-secrets (%input-text octets text))
                                 (funcall on-ok (list (cons "text" redacted) (cons "redactions" count)))))
                   :on-error on-error))

(defun util-tokens-flow (ports request &key on-ok on-error)
  (resolve-input/k ports request "util tokens"
                   :on-input (lambda (octets &key text)
                               (funcall on-ok (aitools.util.domain:text-statistics (%input-text octets text)
                                                                                   (length octets))))
                   :on-error on-error))

(defun %calc-evaluate (expression decimals on-ok on-error)
  (aitools.util.domain:evaluate-expression/k
   expression
   :on-value (lambda (value)
               (let ((exact (aitools.util.domain:format-exact value)))
                 (funcall on-ok (append (list (cons "input" expression)
                                              (cons "result" (aitools.util.domain:format-decimal value decimals)))
                                        (when exact (list (cons "exact" exact)))))))
   :on-error (lambda (offset reason)
               (funcall on-error "input.syntax-error" (format nil "at character ~D: ~A" offset reason)
                        :repairs (list (%schema-repair "util calc"))))))

(defun util-calc-flow (ports expression stdin decimals &key on-ok on-error)
  "EXPRESSION is the positional expression or NIL; STDIN is `--stdin`.
Exactly one must be given. DECIMALS bounds the rounded `result`; `exact`
carries the unrounded rational."
  (declare (type function on-ok on-error))
  (cond
    ((not (<= 0 decimals aitools.util.domain:+calc-max-decimals+))
     (funcall on-error "argument.invalid"
              (format nil "--decimals must be between 0 and ~D" aitools.util.domain:+calc-max-decimals+)
              :repairs (list (aitools.protocol.domain:repair
                              "use-default" "Use the default precision." "aitools util calc '<expr>'"))))
    ((eq (null expression) (not stdin))
     (funcall on-error "argument.invalid" "pass exactly one of an expression argument or --stdin"
              :repairs (list (aitools.protocol.domain:repair
                              "pass-expression" "Pass the expression as one argument." "aitools util calc '<expr>'")
                             (aitools.protocol.domain:repair
                              "pass-stdin" "Read the expression from standard input." "aitools util calc --stdin"))))
    (expression (%calc-evaluate expression decimals on-ok on-error))
    (t
     (resolve-input/k ports (make-input-request :stdin t) "util calc"
                      :on-input (lambda (octets &key text)
                                  (declare (ignore octets))
                                  (%calc-evaluate (string-trim '(#\Space #\Tab #\Newline #\Return) text)
                                                  decimals on-ok on-error))
                      :on-error on-error))))

(defun %count-error (on-error command)
  (funcall on-error "argument.invalid" (format nil "--count must be between 1 and ~D" +util-max-count+)
           :repairs (list (aitools.protocol.domain:repair
                           "use-default" "Generate one value." (format nil "aitools ~A --count 1" command)))))

(defun util-uuid-flow (ports kind count &key on-ok on-error)
  "KIND is \"v4\" or \"v7\". v7 values from one call are strictly increasing."
  (declare (type function on-ok on-error))
  (cond
    ((not (<= 1 count +util-max-count+)) (%count-error on-error "util uuid"))
    ((string= kind "v4")
     (funcall on-ok (list (cons "values" (loop repeat count collect (funcall (util-ports-uuid-v4 ports)))))))
    ((string= kind "v7")
     (let ((state (aitools.util.domain:make-uuid-v7-state)))
       (funcall on-ok
                (list (cons "values"
                            (loop repeat count
                                  collect (aitools.util.domain:next-uuid-v7
                                           state (funcall (util-ports-unix-ms ports))
                                           (funcall (util-ports-random-octets ports)
                                                    aitools.util.domain:+uuid-v7-random-octets+))))))))
    (t (%choice-error on-error "--kind" kind +util-uuid-kinds+ "util uuid"))))

(defun %octet-supplier (ports)
  "A closure returning one random octet per call, drawing from the
RANDOM-OCTETS port in blocks so the port is not called per character."
  (let ((buffer (funcall (util-ports-random-octets ports) 64)) (index 0))
    (lambda ()
      (when (= index (length buffer))
        (setf buffer (funcall (util-ports-random-octets ports) 64) index 0))
      (prog1 (aref buffer index) (incf index)))))

(defun util-random-flow (ports length alphabet count &key on-ok on-error)
  (declare (type function on-ok on-error))
  (cond
    ((not (<= 1 count +util-max-count+)) (%count-error on-error "util random"))
    ((not (<= 1 length +util-max-random-length+))
     (funcall on-error "argument.invalid" (format nil "--length must be between 1 and ~D" +util-max-random-length+)
              :repairs (list (aitools.protocol.domain:repair
                              "use-default" "Use the default length." "aitools util random --length 32"))))
    ((not (aitools.util.domain:random-alphabet-p alphabet))
     (%choice-error on-error "--alphabet" alphabet +util-random-alphabets+ "util random"))
    (t
     (let ((next-octet (%octet-supplier ports)))
       (funcall on-ok (list (cons "values"
                                  (loop repeat count
                                        collect (aitools.util.domain:random-alphabet-string
                                                 alphabet length next-octet)))))))))
