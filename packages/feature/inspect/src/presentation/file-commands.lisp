;;;; packages/feature/inspect/src/presentation/file-commands.lisp
;;;;
;;;; `read`, `info`, `check`, and `diff`: option parsing
;;;; and one flow call each.
(in-package #:aitools.inspect.presentation)

(defun %path-positional ()
  (make-positional :key :path :name "path" :required-p t))

(defun %read-command (ports)
  (make-command
   :name "read" :description (command-summary "read" aitools.data:*inspect-command-schemas*)
   :positionals (list (%path-positional))
   :options (append (selector-options)
                    (list (integer-option "tail" nil "The last N lines.")
                          (value-option "as" "text, hex, or strings." :choices '("text" "hex" "strings") :default "text")
                          (value-option "bytes" "With --as hex: byte span S:E or S:." :value-name "S:E")
                          (integer-option "min-length" 4 "With --as strings: shortest run.")
                          (flag-option "escape-invisible" "Show invisible characters as \\u{XXXX}.")
                          (value-option "encoding" "Decode from this encoding." :value-name "NAME")
                          (integer-option "max-lines" 80 "Maximum lines returned.")
                          (tx-option)))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:read-flow ports
                     (positional-value invocation :path)
                     :tail (option-value invocation :tail)
                     :as (option-value invocation :as)
                     :bytes (option-value invocation :bytes)
                     :min-length (option-value invocation :min-length)
                     :escape-invisible (option-value invocation :escape-invisible)
                     :encoding (option-value invocation :encoding)
                     :max-lines (option-value invocation :max-lines)
                     (append (selector-arguments invocation) (context-arguments invocation))))))

(defun %info-command (ports)
  (make-command
   :name "info" :description (command-summary "info" aitools.data:*inspect-command-schemas*)
   :positionals (list (%path-positional))
   :options (list (value-option "digest" "sha256, sha1, or md5." :choices '("sha256" "sha1" "md5"))
                  (flag-option "allow-missing" "Report exists:false instead of failing.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:info-flow ports
                     (positional-value invocation :path)
                     :digest (option-value invocation :digest)
                     :allow-missing (option-value invocation :allow-missing)
                     (context-arguments invocation)))))

(defun %check-command (ports)
  (make-command
   :name "check" :description (command-summary "check" aitools.data:*inspect-command-schemas*)
   :positionals (list (%path-positional))
   :options (list (value-option "format" "json or lisp." :choices '("json" "lisp"))
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:check-flow ports
                     (positional-value invocation :path)
                     :format (option-value invocation :format)
                     (context-arguments invocation)))))

(defun %diff-command (ports)
  (make-command
   :name "diff" :description (command-summary "diff" aitools.data:*inspect-command-schemas*)
   :positionals (list (make-positional :key :a :name "a" :required-p nil)
                      (make-positional :key :b :name "b" :required-p nil))
   :options (list (value-option "op" "A journal op id." :value-name "OP_ID")
                  (integer-option "context" 3 "Unchanged lines around each hunk." :min 0)
                  (value-option "output" "unified, stat, or set." :choices '("unified" "stat" "set") :default "unified")
                  (flag-option "ignore-whitespace" "Ignore all whitespace.")
                  (flag-option "ignore-eol" "Ignore CR before LF and the final newline.")
                  (integer-option "limit" 100 "Maximum items returned.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:diff-flow ports
                     :a (positional-value invocation :a)
                     :b (positional-value invocation :b)
                     :op (option-value invocation :op)
                     :context (option-value invocation :context)
                     :output (option-value invocation :output)
                     :ignore-whitespace (option-value invocation :ignore-whitespace)
                     :ignore-eol (option-value invocation :ignore-eol)
                     :limit (option-value invocation :limit)
                     (context-arguments invocation)))))

(define-inspect-command "read" nil '%read-command aitools.data:*inspect-command-schemas*)
(define-inspect-command "info" nil '%info-command aitools.data:*inspect-command-schemas*)
(define-inspect-command "check" nil '%check-command aitools.data:*inspect-command-schemas*)
(define-inspect-command "diff" nil '%diff-command aitools.data:*inspect-command-schemas*)
