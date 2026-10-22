;;;; packages/feature/inspect/src/presentation/yaml-commands.lisp
;;;; YAML read commands.
(in-package #:aitools.inspect.presentation)

(defun %yaml-summary (name)
  (command-summary name aitools.data:*inspect-format-command-schemas*))

(defun %yaml-get-command (ports)
  (make-command
   :name "get" :description (%yaml-summary "yaml.get")
   :positionals (list (make-positional :key :path :name "path" :required-p t)
                      (make-positional :key :pointer :name "pointer" :required-p t))
   :options (list (value-option "max-bytes" "Largest rendered value returned."
                                :default "16KiB" :value-name "SIZE")
                  (flag-option "keys" "Return the keys or indexes.")
                  (flag-option "raw" "Return the string value unescaped.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:yaml-get-flow ports
                     (positional-value invocation :path)
                     (positional-value invocation :pointer)
                     :max-bytes (option-value invocation :max-bytes)
                     :keys (option-value invocation :keys)
                     :raw (option-value invocation :raw)
                     (context-arguments invocation)))))

(defun %yaml-select-command (ports)
  (make-command
   :name "select" :description (%yaml-summary "yaml.select")
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
              (apply #'run-flow #'aitools.inspect.application:yaml-select-flow ports
                     (positional-value invocation :path)
                     (positional-value invocation :pointer)
                     :where (option-value invocation :where)
                     :pick (option-value invocation :pick)
                     :sort-by (option-value invocation :sort-by)
                     :desc (option-value invocation :desc)
                     :output (option-value invocation :output)
                     :limit (option-value invocation :limit)
                     (context-arguments invocation)))))

(defun %yaml-diff-command (ports)
  (make-command
   :name "diff" :description (%yaml-summary "yaml.diff")
   :positionals (list (make-positional :key :a :name "a" :required-p t)
                      (make-positional :key :b :name "b" :required-p t))
   :options (list (integer-option "limit" 100 "Maximum operations returned.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:yaml-diff-flow ports
                     (positional-value invocation :a)
                     (positional-value invocation :b)
                     :limit (option-value invocation :limit)
                     (context-arguments invocation)))))

(define-inspect-command "yaml.get" "yaml" '%yaml-get-command
  aitools.data:*inspect-format-command-schemas*)
(define-inspect-command "yaml.select" "yaml" '%yaml-select-command
  aitools.data:*inspect-format-command-schemas*)
(define-inspect-command "yaml.diff" "yaml" '%yaml-diff-command
  aitools.data:*inspect-format-command-schemas*)
