;;;; t/integration/skill-contract-test.lisp
;;;;
;;;; skills/aitools/SKILL.md is what an agent copies invocations from, so every
;;;; example in it must name a command the parser registers and only options
;;;; that command (or the application) accepts. Examples are the lines that
;;;; start with `aitools ` inside fenced code blocks; prose that mentions
;;;; `aitools ...` in backticks is not checked. Each example is resolved
;;;; against the live cl-cli tree from BUILD-APP, the same data
;;;; cli-schema-drift-test.lisp compares with the schema, so a renamed
;;;; command, a removed flag, or an enum value the parser rejects fails here
;;;; instead of in an agent's session.
(in-package #:cl-user)

(defpackage #:aitools.integration.skill-contract-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.skill-contract-test)

(defparameter *minimum-examples* 40
  "A floor, so an extractor that silently stops matching cannot pass vacuously.")

(defun %skill-text ()
  (uiop:read-file-string (asdf:system-relative-pathname "aitools" "skills/aitools/SKILL.md")))

(defun %trim (string)
  (string-trim '(#\Space #\Tab #\Return) string))

(defun skill-command-lines (text)
  "(LINE-NUMBER . COMMAND) for every line of TEXT that starts with `aitools `
inside a fenced code block."
  (let ((in-fence nil) (commands '()))
    (loop for line in (uiop:split-string text :separator '(#\Newline))
          for number from 1
          for trimmed = (%trim line)
          do (cond ((uiop:string-prefix-p "```" trimmed) (setf in-fence (not in-fence)))
                   ((and in-fence (uiop:string-prefix-p "aitools " trimmed))
                    (push (cons number trimmed) commands))))
    (nreverse commands)))

(defun shell-words (line)
  "Split LINE like a POSIX shell: whitespace separates, single quotes are
literal, double quotes allow backslash escapes. Stops at the first unquoted
redirection, pipe, or command separator, which belongs to the shell and not
to aitools."
  ;; STARTED separates an empty quoted word ('') from no word at all.
  (let ((words '()) (word '()) (started nil) (quote-char nil) (n (length line)))
    (flet ((flush ()
             (when started (push (coerce (nreverse word) 'string) words))
             (setf word '() started nil)))
      (loop with i = 0
            while (< i n)
            do (let ((c (char line i)))
                 (cond ((eql quote-char #\') (if (char= c #\') (setf quote-char nil) (push c word)))
                       ((eql quote-char #\")
                        (cond ((char= c #\") (setf quote-char nil))
                              ((and (char= c #\\) (< (1+ i) n)) (incf i) (push (char line i) word))
                              (t (push c word))))
                       ((member c '(#\' #\")) (setf quote-char c started t))
                       ((member c '(#\Space #\Tab)) (flush))
                       ((member c '(#\| #\< #\> #\; #\&)) (loop-finish))
                       (t (push c word) (setf started t))))
               (incf i))
      (flush))
    (nreverse words)))

(defun %leaf-commands (app)
  "Hash table: typed name (`read`, `json get`) -> cl-cli command, for every runnable command."
  (let ((table (make-hash-table :test #'equal)))
    (labels ((walk (command prefix)
               (let ((name (if prefix (format nil "~A ~A" prefix (cl-cli:command-name command)) (cl-cli:command-name command))))
                 (if (cl-cli:command-subcommands command)
                     (dolist (subcommand (cl-cli:command-subcommands command)) (walk subcommand name))
                     (setf (gethash name table) command)))))
      (dolist (command (cl-cli:app-commands app)) (walk command nil)))
    table))

(defun %option-index (options)
  "Alist of (\"--spelling\" OPTION NEGATED-P) over OPTIONS."
  (loop for option in options
        append (mapcar (lambda (name) (list (format nil "--~A" name) option nil)) (cl-cli:option-names option))
        append (mapcar (lambda (name) (list (format nil "--~A" name) option t)) (cl-cli:option-negated-names option))))

(defun %value-count (option negated)
  "How many following words OPTION consumes: 0, a fixed count, or :VARIADIC."
  (let ((count (cl-cli:option-value-count option)))
    (cond ((or negated (member (cl-cli:option-kind option) '(:flag :boolean :count))) 0)
          ((integerp count) count)
          ((member count '(:+ :*)) :variadic)
          (t 1))))

(defun %choice-problem (option value)
  "A problem string when OPTION has choices and the literal VALUE is not one."
  (let ((choices (cl-cli:option-choices option)))
    (and choices (not (find #\$ value))
         (not (member value choices :test #'string=))
         (format nil "--~A ~S is not one of ~{~A~^, ~}" (first (cl-cli:option-names option)) value choices))))

(defun %option-word-p (word)
  (and (> (length word) 2) (uiop:string-prefix-p "--" word)))

(defun example-problems (words leaves app)
  "Problems with one example's WORDS, as strings; NIL when it resolves."
  (let ((globals (%option-index (cl-cli:app-global-options app)))
        (rest (rest words))
        (problems '()))
    (unless (equal (first words) "aitools")
      (return-from example-problems (list "does not start with aitools")))
    ;; Global options may precede the command name.
    (loop while (and rest (%option-word-p (first rest)))
          do (let* ((word (pop rest))
                    (equals (position #\= word))
                    (entry (assoc (subseq word 0 (or equals (length word))) globals :test #'string=)))
               (cond ((member word '("--help" "--version") :test #'string=))
                     ((null entry)
                      (return-from example-problems (list (format nil "unknown global option ~A" word))))
                     ((not equals) (pop rest)))))
    (let* ((head (pop rest))
           (typed (cond ((null head) nil)
                        ((gethash head leaves) head)
                        ((and rest (gethash (format nil "~A ~A" head (first rest)) leaves))
                         (format nil "~A ~A" head (pop rest)))))
           (command (and typed (gethash typed leaves))))
      (unless command
        (return-from example-problems
          (list (format nil "unknown command ~A~@[ ~A~]" head
                        (and (loop for name being the hash-keys of leaves
                                   thereis (uiop:string-prefix-p (format nil "~A " head) name))
                             (first rest))))))
      (let ((index (append (%option-index (cl-cli:command-options command)) globals)))
        (loop while rest
              do (let ((word (pop rest)))
                   (cond ((string= word "--") (loop-finish))
                         ((string= word "--help"))
                         ((%option-word-p word)
                          (let* ((equals (position #\= word))
                                 (spelling (subseq word 0 (or equals (length word))))
                                 (entry (assoc spelling index :test #'string=)))
                            (if (null entry)
                                (push (format nil "~A has no option ~A" typed spelling) problems)
                                (destructuring-bind (option negated) (rest entry)
                                  (let ((count (%value-count option negated)))
                                    (cond (equals
                                           (let ((problem (%choice-problem option (subseq word (1+ equals)))))
                                             (when problem (push problem problems))))
                                          ((eq count :variadic)
                                           (loop while (and rest (not (%option-word-p (first rest))))
                                                 do (pop rest)))
                                          ((< (length rest) count)
                                           (push (format nil "~A needs ~D value~:P" spelling count) problems)
                                           (setf rest nil))
                                          ((plusp count)
                                           (let ((problem (%choice-problem option (first rest))))
                                             (when problem (push problem problems)))
                                           (setf rest (nthcdr count rest)))))))))))))
      (nreverse problems))))

(defun skill-drift (text)
  "(VALUES PROBLEMS EXAMPLE-COUNT) for SKILL.md TEXT against the live parser.
Each problem reads \"LINE: problem (example)\"."
  (let* ((app (aitools/cli:build-app))
         (leaves (%leaf-commands app))
         (lines (skill-command-lines text)))
    (values (loop for (number . line) in lines
                  append (mapcar (lambda (problem) (format nil "~D: ~A (~A)" number problem line))
                                 (example-problems (shell-words line) leaves app)))
            (length lines))))

(defun frontmatter-version (text)
  "The `version:` value of TEXT's leading YAML frontmatter, or NIL."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (when (string= (%trim (first lines)) "---")
      (loop for line in (rest lines)
            until (string= (%trim line) "---")
            when (uiop:string-prefix-p "version:" line)
              return (%trim (subseq line (length "version:")))))))

(describe "skills/aitools/SKILL.md matches the CLI"
  (it "extracts more examples than the floor"
    (expect (> (nth-value 1 (skill-drift (%skill-text))) *minimum-examples*) :to-be t))

  (it "names only registered commands, their options, and accepted enum values"
    ;; A joined string, so a failure prints every drifted line at once.
    (expect (format nil "~{~A~%~}" (skill-drift (%skill-text))) :to-equal ""))

  (it "declares the version aitools.asd declares"
    (expect (frontmatter-version (%skill-text))
            :to-equal (asdf:component-version (asdf:find-system "aitools"))))

  (it "reports an unknown command, an unknown option, and a rejected enum value by line"
    (let ((problems (skill-drift (format nil "```sh~%~
                                              aitools frobnicate x~%~
                                              aitools read a.txt --no-such-flag~%~
                                              aitools search x --output nonsense~%~
                                              aitools json get a.json /k --raw~%~
                                              aitools edit a.txt --between '^a' '^b' --new ''~%~
                                              aitools run --timeout 5s -- make --no-such-flag~%~
                                              ```~%"))))
      (expect (mapcar (lambda (problem) (subseq problem 0 (position #\: problem))) problems)
              :to-equal '("2" "3" "4")))))
