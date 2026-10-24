;;;; packages/feature/edit/src/application/command-spec.lisp
;;;;
;;;; The expanded command table (data/application/edit/command-spec-data.lisp
;;;; plus the shared option groups), the canonical argv a write records in
;;;; the journal and tx ops, and the parser that reads such an argv back for
;;;; `tx rebase`. Presentation builds its cl-cli options from the same
;;;; table, so a recorded argv always parses the way the live one did.
(in-package #:aitools.edit.application)

(defun %expand-spec (spec)
  (let ((options (copy-list (getf spec :options))))
    (dolist (group (getf spec :include))
      (dolist (option (cdr (assoc group aitools.data:*edit-option-groups*)))
        (unless (find (getf option :key) options :key (lambda (o) (getf o :key)))
          (setf options (append options (list option))))))
    (list* :options
           options
           (loop for (key value) on spec by #'cddr
                 unless (eq key :options)
                   append (list key value)))))

(defparameter *edit-commands*
  (mapcar #'%expand-spec aitools.data:*edit-command-specs*)
  "Every edit command's spec with its option groups expanded.")

(defun edit-command-specs () *edit-commands*)

(defun find-command-spec (name)
  (find name
        *edit-commands*
        :key
        (lambda (spec)
          (getf spec :name))
        :test
        #'string=))

(defun command-words (name)
  "The argv words naming dispatch NAME: \"json.set\" -> (\"json\" \"set\")."
  (labels ((split-at (start)
             (let ((dot (position #\. name :start start)))
               (if dot
                   (cons (subseq name start dot)
                         (split-at (1+ dot)))
                   (list (subseq name start))))))
    (split-at 0)))

(defun command-display-name (name)
  (format
    nil
    "~{~A~^ ~}"
    (command-words name)))

(defun options-argv (name positionals options)
  "The canonical argv of command NAME: its words, POSITIONALS, then each
option of OPTIONS (a plist keyed by option :KEY) in spec order, then
POSITIONALS, after `--` when one of them starts with `-`. --dry-run, --tx
and --stdin are left out: the stdin input is recorded as --stdin-data.
A command outside this table (another context writing through the
pipeline, such as `util decode --to`) records OPTIONS in their plist order,
T as a flag and anything else as a value."
  (let ((spec (find-command-spec name)))
    (append (command-words name)
            (loop for option in (if spec
                                    (getf spec :options)
                                    (loop for (key value) on options by #'cddr
                                          collect (list :key
                                                        key
                                                        :name
                                                        (string-downcase (symbol-name key))
                                                        :kind
                                                        (if (eq value t)
                                                            :flag
                                                            :value))))
                  for key = (getf option :key)
                  for value = (getf options key)
                  unless (or (null value) (member key '(:dry-run :tx :stdin)))
                    append (let ((flag
                                  (format
                                    nil
                                    "--~A"
                                    (getf option :name))))
                             (ecase (getf option :kind)
                               (:flag (list flag))
                               (:value (list flag (princ-to-string value)))
                               (:multi
                                (loop for item in value
                                      append (list flag (princ-to-string item))))
                               (:pair
                                (list flag
                                      (princ-to-string (first value))
                                      (princ-to-string (second value)))))))
            (when
                (some
                 (lambda (positional)
                   (and (plusp (length positional)) (char= (char positional 0) #\-)))
                 positionals)
              (list "--"))
            positionals)))

(defun %parse-recorded-option (option token tokens options)
  (let ((key (getf option :key)))
    (labels ((set-option (value)
               (if (loop for tail on options by #'cddr
                         thereis (eq (car tail) key))
                   (setf (getf options key) value)
                   (setf options (append options (list key value))))))
      (ecase (getf option :kind)
        (:flag
         (set-option t)
         (values nil tokens options))
        (:value
         (if (null tokens)
             (values (format nil "~A needs a value" token) tokens options)
             (progn
               (set-option (pop tokens))
               (values nil tokens options))))
        (:multi
         (if (null tokens)
             (values (format nil "~A needs a value" token) tokens options)
             (progn
               (set-option (append (getf options key) (list (pop tokens))))
               (values nil tokens options))))
        (:pair
         (if (null (rest tokens))
             (values (format nil "~A needs two values" token) tokens options)
             (progn
               (set-option (list (pop tokens) (pop tokens)))
               (values nil tokens options))))))))

(aitools.kernel.domain:define-flow
 (parse-recorded-argv argv &key on-parsed on-invalid)
 "Parse a canonical ARGV (see OPTIONS-ARGV) back into (ON-PARSED name
positionals options), or call ON-INVALID (message)."
 (let* ((two
         (and (rest argv)
              (format
               nil
               "~A.~A"
               (first argv)
               (second argv))))
        (spec (or (and two (find-command-spec two)) (find-command-spec (first argv)))))
   (if (null spec)
       (aitools.kernel.domain:flow-continue
        :on-invalid
        (format
         nil
         "no edit command for argv ~S"
         argv))
       (let ((tokens (nthcdr (length (command-words (getf spec :name))) argv))
             (positionals '())
             (options '())
             (invalid-message nil))
         (loop while (and tokens (null invalid-message))
               do (let* ((token (pop tokens))
                         (option
                           (and (> (length token) 2)
                                (string= token "--" :end1 2)
                                (find (subseq token 2)
                                      (getf spec :options)
                                      :key
                                      (lambda (o)
                                        (getf o :name))
                                      :test
                                      #'string=))))
                    (cond
                      ((string= token "--")
                       (setf positionals (append (reverse tokens) positionals)
                             tokens nil))
                      ((null option)
                       (push token positionals))
                      (t
                       (multiple-value-setq (invalid-message tokens options)
                         (%parse-recorded-option option token tokens options))))))
         (if invalid-message
             (aitools.kernel.domain:flow-continue
              :on-invalid
              invalid-message)
             (aitools.kernel.domain:flow-continue
              :on-parsed
              (getf spec :name)
              (nreverse positionals)
              options))))))
