;;;; packages/feature/inspect/src/presentation/toml-commands.lisp
;;;; TOML read commands.
(in-package #:aitools.inspect.presentation)

(defun %toml-summary (name)
  (command-summary name aitools.data:*inspect-format-command-schemas*))

(defun %toml-get-command (ports)
  (make-command
   :name "get" :description (%toml-summary "toml.get")
   :positionals (list (make-positional :key :path :name "path" :required-p t)
                      (make-positional :key :pointer :name "pointer" :required-p t))
   :options (list (value-option "max-bytes" "Largest rendered value returned."
                                :default "16KiB" :value-name "SIZE")
                  (flag-option "keys" "Return the keys or indexes.")
                  (flag-option "raw" "Return the string value unescaped.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:toml-get-flow ports
                     (positional-value invocation :path)
                     (positional-value invocation :pointer)
                     :max-bytes (option-value invocation :max-bytes)
                     :keys (option-value invocation :keys)
                     :raw (option-value invocation :raw)
                     (context-arguments invocation)))))

(defun %toml-select-command (ports)
  (make-command
   :name "select" :description (%toml-summary "toml.select")
   :positionals (list (make-positional :key :path :name "path" :required-p t)
                      (make-positional :key :pointer :name "pointer" :required-p t))
   :options (list (%repeated-option "where" "Condition <rel-pointer><op><json-value>."
                                    :value-name "COND")
                  (%repeated-option "pick" "Relative pointer to return." :value-name "POINTER")
                  (value-option "sort-by" "Relative pointer to order by." :value-name "POINTER")
                  (flag-option "desc" "Descending order.")
                  (value-option "output" "items or count." :choices '("items" "count")
                                :default "items")
                  (integer-option "limit" 50 "Maximum items returned.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:toml-select-flow ports
                     (positional-value invocation :path)
                     (positional-value invocation :pointer)
                     :where (option-value invocation :where)
                     :pick (option-value invocation :pick)
                     :sort-by (option-value invocation :sort-by)
                     :desc (option-value invocation :desc)
                     :output (option-value invocation :output)
                     :limit (option-value invocation :limit)
                     (context-arguments invocation)))))

(defun %toml-diff-command (ports)
  (make-command
   :name "diff" :description (%toml-summary "toml.diff")
   :positionals (list (make-positional :key :a :name "a" :required-p t)
                      (make-positional :key :b :name "b" :required-p t))
   :options (list (integer-option "limit" 100 "Maximum operations returned.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:toml-diff-flow ports
                     (positional-value invocation :a)
                     (positional-value invocation :b)
                     :limit (option-value invocation :limit)
                     (context-arguments invocation)))))

(define-inspect-command "toml.get" "toml" '%toml-get-command
  aitools.data:*inspect-format-command-schemas*)
(define-inspect-command "toml.select" "toml" '%toml-select-command
  aitools.data:*inspect-format-command-schemas*)
(define-inspect-command "toml.diff" "toml" '%toml-diff-command
  aitools.data:*inspect-format-command-schemas*)
