;;;; packages/feature/search/src/presentation/commands.lisp
;;;;
;;;; The cl-cli commands `search`, `find`, `overview`, and `code
;;;; outline|defs|refs`. Each handler turns the parsed invocation into one
;;;; AITOOLS.SEARCH.APPLICATION flow call and returns the COMMAND-RESULT that
;;;; call produced. `--root` is the composition root's global option.
(in-package #:aitools.search.presentation)

(defun %schema (name &key scan-args)
  (let ((entry (find name aitools.data:*search-command-schemas*
                     :key (lambda (entry) (getf entry :name)) :test #'string=)))
    (aitools.protocol.domain:make-command-schema
     name (getf entry :summary)
     :description (getf entry :description)
     :args (append (getf entry :args) (when scan-args aitools.data:*search-scan-args*))
     :output-fields (getf entry :output-fields)
     :error-codes (getf entry :error-codes))))

(defun %summary (name)
  (aitools.protocol.domain:command-schema-summary (%schema name)))

(defun %run-flow (flow &rest arguments)
  "Call FLOW with ARGUMENTS followed by the three command-result
continuations, returning the resulting COMMAND-RESULT."
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun %flag (name description)
  (make-option :name name :kind :flag :description description))

(defun %value (name description &rest keys)
  (apply #'make-option :name name :kind :value :description description keys))

(defun %limit-option (default)
  (%value "limit" "Maximum results returned." :type :integer :min 1 :default default))

(defun %tx-option ()
  (%value "tx" "Read through this tx." :value-name "TX"))

(defun %scan-options ()
  (list (%value "glob" "Only paths matching this glob; repeatable." :multiple-p t :value-name "GLOB")
        (%value "lang" "Only files of this language." :value-name "LANG")
        (%flag "no-ignore" "Do not apply .gitignore or the builtin excludes.")
        (%value "skip-larger-than" "Skip files larger than this size." :value-name "SIZE")
        (%value "newer" "Only entries newer than this path or duration." :value-name "PATH|DURATION")
        (%tx-option)))

(defun %scan-arguments (invocation)
  (list :glob (option-value invocation :glob)
        :lang (option-value invocation :lang)
        :no-ignore (and (option-value invocation :no-ignore) t)
        :skip-larger-than (option-value invocation :skip-larger-than)
        :newer (option-value invocation :newer)
        :tx (option-value invocation :tx)
        :root (option-value invocation :root)))

(defparameter *choice-keywords*
  '(("blocks" . :blocks) ("matches" . :matches) ("count" . :count) ("files" . :files)
    ("files-without-match" . :files-without-match) ("path" . :path) ("mtime" . :mtime) ("size" . :size)
    ("flat" . :flat) ("tree" . :tree) ("file" . :file) ("dir" . :directory) ("symlink" . :symlink))
  "The keyword each `--output`, `--sort`, and `--type` choice stands for.")

(defun %keyword (text)
  (and text (cdr (assoc text *choice-keywords* :test #'string=))))

;;; ------------------------------------------------------------ search

(defun %path-values (invocation)
  (positional-value invocation :paths))

(defun %search-command (ports)
  (make-command
   :name "search" :description (%summary "search")
   ;; One :REST-P positional for the paths. cl-cli v1.4.0 interleaves
   ;; options with a rest positional (a token after the first path is still
   ;; read as an option), so `search foo src --output count` no longer
   ;; searches a path named `--output`.
   :positionals (list (make-positional :key :pattern :name "pattern" :required-p nil)
                      (make-positional :key :paths :name "path" :rest-p t :required-p nil))
   :options (append
             (list (%flag "fixed" "Treat the pattern as literal text.")
                   (%flag "ignore-case" "Case-insensitive matching.")
                   (%flag "word" "Match only at word boundaries.")
                   (%flag "line-regexp" "Match only whole lines.")
                   (%value "pattern" "A pattern; repeat for any of several." :multiple-p t :value-name "PATTERN")
                   (%flag "stdin" "Read the pattern from standard input.")
                   (%flag "invert" "Select lines that do not match.")
                   (%flag "multiline" "Let matches span lines.")
                   (%value "context" "Context lines around each selected line." :type :integer :min 0 :default 2)
                   (%value "before" "Context lines before; overrides --context." :type :integer :min 0)
                   (%value "after" "Context lines after; overrides --context." :type :integer :min 0)
                   (%value "output" "Result shape." :default "blocks"
                           :choices '("blocks" "matches" "count" "files" "files-without-match"))
                   (%limit-option 15))
             (%scan-options))
   :handler
   (lambda (invocation)
     (let* ((positional (positional-value invocation :pattern))
            (rest (%path-values invocation))
            (option-patterns (option-value invocation :pattern))
            (stdin (and (option-value invocation :stdin) t))
            ;; With --pattern or --stdin every positional is a path.
            (patterns-elsewhere (or option-patterns stdin)))
       (apply #'%run-flow #'aitools.search.application:search/k ports
              :patterns (if patterns-elsewhere option-patterns (and positional (list positional)))
              :paths (if (and patterns-elsewhere positional) (cons positional rest) rest)
              :stdin stdin
              :fixed (and (option-value invocation :fixed) t)
              :ignore-case (and (option-value invocation :ignore-case) t)
              :word (and (option-value invocation :word) t)
              :line-regexp (and (option-value invocation :line-regexp) t)
              :invert (and (option-value invocation :invert) t)
              :multiline (and (option-value invocation :multiline) t)
              :context (option-value invocation :context)
              :before (option-value invocation :before)
              :after (option-value invocation :after)
              :output (%keyword (option-value invocation :output))
              :limit (option-value invocation :limit)
              (%scan-arguments invocation))))))

;;; ------------------------------------------------------------ find

(defun %find-command (ports)
  (make-command
   :name "find" :description (%summary "find")
   :positionals (list (make-positional :key :pattern :name "pattern" :required-p nil)
                      (make-positional :key :path :name "path" :required-p nil))
   :options (append
             (list (%value "type" "Only entries of this kind." :choices '("file" "dir" "symlink"))
                   (%value "depth" "Maximum depth below the start." :type :integer :min 0)
                   (%value "sort" "Ordering." :choices '("path" "mtime" "size") :default "path")
                   (%value "min-size" "Minimum size." :value-name "SIZE")
                   (%value "max-size" "Maximum size." :value-name "SIZE")
                   (%flag "empty" "Only empty files and directories.")
                   (%flag "executable" "Only executable files.")
                   (%value "output" "flat or tree." :choices '("flat" "tree") :default "flat")
                   (%flag "sizes" "Sum file sizes into directories.")
                   (%limit-option 50))
             (%scan-options))
   :handler
   (lambda (invocation)
     (let ((type (option-value invocation :type)))
       (apply #'%run-flow #'aitools.search.application:find/k ports
              :pattern (positional-value invocation :pattern)
              :path (positional-value invocation :path)
              :type (%keyword type)
              :depth (option-value invocation :depth)
              :sort (%keyword (option-value invocation :sort))
              :min-size (option-value invocation :min-size)
              :max-size (option-value invocation :max-size)
              :empty (and (option-value invocation :empty) t)
              :executable (and (option-value invocation :executable) t)
              :output (%keyword (option-value invocation :output))
              :sizes (and (option-value invocation :sizes) t)
              :limit (option-value invocation :limit)
              (%scan-arguments invocation))))))

;;; ------------------------------------------------------------ code, overview

(defun %outline-command (ports)
  (make-command
   :name "outline" :description (%summary "code.outline")
   :positionals (list (make-positional :key :path :name "path" :required-p t))
   :options (list (%limit-option 200) (%tx-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.search.application:code-outline/k ports
                         :path (positional-value invocation :path)
                         :limit (option-value invocation :limit)
                         :tx (option-value invocation :tx)
                         :root (option-value invocation :root)))))

(defun %defs-command (ports)
  (make-command
   :name "defs" :description (%summary "code.defs")
   :positionals (list (make-positional :key :name :name "name" :required-p t)
                      (make-positional :key :path :name "path" :required-p nil))
   :options (list (%flag "prefix" "Match names starting with NAME.")
                  (%value "kind" "Only definitions of this kind." :value-name "KIND")
                  (%limit-option 50) (%tx-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.search.application:code-defs/k ports
                         :name (positional-value invocation :name)
                         :path (positional-value invocation :path)
                         :prefix (and (option-value invocation :prefix) t)
                         :kind (option-value invocation :kind)
                         :limit (option-value invocation :limit)
                         :tx (option-value invocation :tx)
                         :root (option-value invocation :root)))))

(defun %refs-command (ports)
  (make-command
   :name "refs" :description (%summary "code.refs")
   :positionals (list (make-positional :key :name :name "name" :required-p t)
                      (make-positional :key :path :name "path" :required-p nil))
   :options (list (%limit-option 50) (%tx-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.search.application:code-refs/k ports
                         :name (positional-value invocation :name)
                         :path (positional-value invocation :path)
                         :limit (option-value invocation :limit)
                         :tx (option-value invocation :tx)
                         :root (option-value invocation :root)))))

(defun %overview-command (ports)
  (make-command
   :name "overview" :description (%summary "overview")
   :positionals (list (make-positional :key :path :name "path" :required-p nil))
   :options (list (%limit-option 30)
                  (%flag "no-ignore" "Include ignored files.")
                  (%tx-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.search.application:overview/k ports
                         :path (positional-value invocation :path)
                         :limit (option-value invocation :limit)
                         :no-ignore (and (option-value invocation :no-ignore) t)
                         :tx (option-value invocation :tx)
                         :root (option-value invocation :root)))))

(defun register-search-commands (registry ports)
  "Register `search`, `find`, `overview`, and `code outline|defs|refs` on
REGISTRY. PORTS is the AITOOLS.SEARCH.APPLICATION:SEARCH-PORTS the
composition root built."
  (loop for (name group builder scan-args) in (list (list "search" nil #'%search-command t)
                                                    (list "find" nil #'%find-command t)
                                                    (list "overview" nil #'%overview-command nil)
                                                    (list "code.outline" "code" #'%outline-command nil)
                                                    (list "code.defs" "code" #'%defs-command nil)
                                                    (list "code.refs" "code" #'%refs-command nil))
        do (aitools.protocol.application:register-command
            registry :name name :group group
                     :cli-command (funcall builder ports)
                     :schema (%schema name :scan-args scan-args)))
  registry)
