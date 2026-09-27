;;;; t/integration/cli-schema-drift-test.lisp
;;;;
;;;; The `schema` command's output must describe what the parser accepts. For every command
;;;; in the finalized cl-cli app, the options (by name, a negatable flag
;;;; counting as documented by either spelling) and the number of positionals
;;;; the parser declares must equal the `args` of `schema <command>`.
;;;; Positionals are compared by count only: cl-cli keeps a positional's key,
;;;; not the display name the schema shows (`src`, `pattern replacement
;;;; [path...]`).
(in-package #:cl-user)

(defpackage #:aitools.integration.cli-schema-drift-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.cli-schema-drift-test)

(defparameter *known-drift*
  ;; (command reason): temporary exceptions, each removed when its
  ;; drift is fixed. No command drifts today; the list is empty and
  ;; every registered command's schema must match its parser.
  '())

(defun %option-spellings (option)
  (mapcar (lambda (name) (format nil "~:[--~;-~]~A" (= (length name) 1) name))
          (append (cl-cli:option-names option) (cl-cli:option-negated-names option))))

(defun %leaf-commands (app)
  "(typed-name . cl-cli command) for every runnable command of APP."
  (let ((leaves '()))
    (labels ((walk (command prefix)
               (let ((name (if prefix (format nil "~A ~A" prefix (cl-cli:command-name command)) (cl-cli:command-name command))))
                 (if (cl-cli:command-subcommands command)
                     (dolist (subcommand (cl-cli:command-subcommands command)) (walk subcommand name))
                     (push (cons name command) leaves)))))
      (dolist (command (cl-cli:app-commands app)) (walk command nil)))
    (nreverse leaves)))

(defun %schema-args (registry typed-name)
  (let ((schema (aitools.protocol.application:find-command-schema registry (substitute #\. #\Space typed-name))))
    (and schema (mapcar (lambda (arg) (getf arg :name)) (aitools.protocol.domain:command-schema-args schema)))))

(defun command-drift (registry typed-name command)
  "NIL when COMMAND's parser and its schema agree, else a string saying how
they differ."
  (let* ((args (%schema-args registry typed-name))
         (schema-options (remove-if-not (lambda (name) (eql 0 (search "-" name))) args))
         (schema-positionals (- (length args) (length schema-options)))
         (options (cl-cli:command-options command))
         (undocumented (loop for option in options
                             for spellings = (%option-spellings option)
                             unless (intersection spellings schema-options :test #'string=)
                               collect (first spellings)))
         (unknown (set-difference schema-options (mapcan #'%option-spellings options) :test #'string=))
         (positionals (length (cl-cli:command-positionals command))))
    (when (or undocumented unknown (/= positionals schema-positionals))
      (format nil "~A: parser-only options ~S, schema-only options ~S, ~D parser positionals vs ~D in schema"
              typed-name undocumented unknown positionals schema-positionals))))

(describe "aitools schema matches the parser"
  (it "gives every command's options and positionals in schema <command>'s args"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (let ((leaves (%leaf-commands app)))
        (expect (> (length leaves) 80) :to-be t)
        (expect (loop for (name . command) in leaves
                      for drift = (command-drift registry name command)
                      when (and drift (not (assoc name *known-drift* :test #'string=)))
                        collect drift)
                :to-equal nil))))

  (it "lists only exceptions that still drift"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (let ((leaves (%leaf-commands app)))
        (expect (loop for (name) in *known-drift*
                      for command = (cdr (assoc name leaves :test #'string=))
                      unless (and command (command-drift registry name command))
                        collect name)
                :to-equal nil)))))
