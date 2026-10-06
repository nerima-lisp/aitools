;;;; packages/feature/util/src/application/input.lisp
;;;;
;;;; The input rule for the single-input util commands: exactly one of
;;;; `--content <text>`, `--content-file <path>` (read as bytes, never
;;;; decoded first), or `--stdin` (read as UTF-8 text). Standard input is
;;;; read only when `--stdin` was given, so a forgotten input fails at once
;;;; instead of waiting on a terminal.
(in-package #:aitools.util.application)

(defstruct (input-request (:constructor make-input-request (&key content content-file stdin))
                          (:copier nil))
  (content nil :type (or null string) :read-only t)
  (content-file nil :type (or null string) :read-only t)
  (stdin nil :type boolean :read-only t))

(defun %input-repairs (command)
  (list (repair
         "pass-content" "Pass the input text inline." (format nil "aitools ~A --content '<text>'" command))
        (repair
         "pass-file" "Read the input bytes from a file." (format nil "aitools ~A --content-file <path>" command))
        (repair
         "pass-stdin" "Read the input text from standard input." (format nil "aitools ~A --stdin" command))))

(defun resolve-input/k (ports request command &key on-input on-error)
  "Read REQUEST's one input source. Calls ON-INPUT with (OCTETS &key TEXT):
TEXT is the input as a string for `--content` and `--stdin`, and absent for
`--content-file`, whose bytes are never decoded here. Calls ON-ERROR with
the command-result error arguments otherwise. COMMAND is the invoked command
line prefix (\"util encode base64\") used to build repairs."
  (declare (type function on-input on-error))
  (let ((given (count-if #'identity (list (input-request-content request)
                                          (input-request-content-file request)
                                          (input-request-stdin request)))))
    (cond
      ((zerop given)
       (funcall on-error "argument.invalid" "no input given: pass one of --content, --content-file, or --stdin"
                :repairs (%input-repairs command)))
      ((> given 1)
       (funcall on-error "argument.invalid" "--content, --content-file, and --stdin are mutually exclusive"
                :repairs (%input-repairs command)))
      ((input-request-content request)
       (let ((text (input-request-content request)))
         (funcall on-input (aitools.util.domain:utf-8-octets text) :text text)))
      ((input-request-content-file request)
       (let ((path (input-request-content-file request)))
         (funcall (util-ports-read-file-octets ports) path +util-max-input-bytes+
                  :on-octets (lambda (octets) (funcall on-input octets))
                  :on-missing (lambda ()
                                (funcall on-error "input.not-found" (format nil "no such file: ~A" path)
                                         :repairs (%input-repairs command)))
                  :on-too-large (lambda ()
                                  (funcall on-error "argument.invalid"
                                           (format nil "~A exceeds the ~D byte input limit" path +util-max-input-bytes+)
                                           :repairs (%input-repairs command)))
                  :on-failure (lambda (message)
                                (funcall on-error "environment.io" (format nil "cannot read ~A: ~A" path message)
                                         :repairs (%input-repairs command))))))
      (t
       (funcall (util-ports-read-stdin-octets ports) +util-max-input-bytes+
                :on-octets
                (lambda (octets)
                  (aitools.util.domain:octets->utf-8/k
                   octets
                   :on-text (lambda (text) (funcall on-input octets :text text))
                   :on-invalid (lambda (offset)
                                 (funcall on-error "input.not-utf8"
                                          (format nil "standard input is not valid UTF-8 at byte ~D" offset)
                                          :repairs (list
                                                    (repair
                                                     "pass-file" "Pass bytes through a file instead; --content-file does not decode."
                                                     (format nil "aitools ~A --content-file <path>" command)))))))
                :on-too-large (lambda ()
                                (funcall on-error "argument.invalid"
                                         (format nil "standard input exceeds the ~D byte input limit" +util-max-input-bytes+)
                                         :repairs (%input-repairs command)))
                :on-failure (lambda (message)
                              (funcall on-error "environment.io" (format nil "cannot read standard input: ~A" message)
                                       :repairs (%input-repairs command))))))))
