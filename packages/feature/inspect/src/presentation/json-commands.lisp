;;;;
;;;; Structured document read commands.
(in-package #:aitools.inspect.presentation)

(defun %run-doc-flow (ports invocation action paths arguments flow-options)
  (apply (function run-flow)
         (append
           (list (function aitools.inspect.application:doc-inspect-flow)
                 ports action paths arguments
                 (option-value invocation :format)
                 flow-options)
           (context-arguments invocation))))

(defun %repeated-option (name description &key value-name)
  (make-option :name name :kind :value :multiple-p t :value-name value-name :description description))

(defmacro %define-doc-command (name action &key positionals options handler)
  (let ((summary-key (concatenate 'string "doc." action)))
    `(defun ,name (ports)
       (make-command
        :name ,action
        :description (command-summary ,summary-key
                                       aitools.data:*inspect-format-command-schemas*)
        :positionals ,positionals
        :options ,options
        :handler ,handler))))

(%define-doc-command %doc-get-command "get"
   :positionals (list (make-positional :key :path :name "path" :required-p t)
                      (make-positional :key :pointer :name "pointer" :required-p t))
   :options (list (value-option "format" "Document format; defaults from the file extension."
                                :choices aitools.data:*document-format-choices*)
                  (value-option "max-bytes" "Largest rendered value returned."
                                :default "16KiB" :value-name "SIZE")
                  (flag-option "keys" "Return the keys or indexes.")
                  (flag-option "raw" "Return the string value unescaped.")
                  (tx-option))
   :handler (lambda (invocation)
              (%run-doc-flow
               ports invocation "get"
               (list (positional-value invocation :path))
               (list (positional-value invocation :path)
                     (positional-value invocation :pointer))
               (list :max-bytes (option-value invocation :max-bytes)
                     :keys (option-value invocation :keys)
                     :raw (option-value invocation :raw)))))

(%define-doc-command %doc-select-command "select"
   :positionals (list (make-positional :key :path :name "path" :required-p t)
                      (make-positional :key :pointer :name "pointer" :required-p t))
   :options (list (value-option "format" "Document format; defaults from the file extension."
                                :choices aitools.data:*document-format-choices*)
                  (%repeated-option "where" "Condition <rel-pointer><op><json-value>."
                                    :value-name "COND")
                  (%repeated-option "pick" "Relative pointer to return." :value-name "POINTER")
                  (value-option "sort-by" "Relative pointer to order by." :value-name "POINTER")
                  (flag-option "desc" "Descending order.")
                  (value-option "output" "items or count." :choices '("items" "count")
                                :default "items")
                  (integer-option "limit" 50 "Maximum items returned.")
                  (tx-option))
   :handler (lambda (invocation)
              (%run-doc-flow
               ports invocation "select"
               (list (positional-value invocation :path))
               (list (positional-value invocation :path)
                     (positional-value invocation :pointer))
               (list :where (option-value invocation :where)
                     :pick (option-value invocation :pick)
                     :sort-by (option-value invocation :sort-by)
                     :desc (option-value invocation :desc)
                     :output (option-value invocation :output)
                     :limit (option-value invocation :limit)))))

(%define-doc-command %doc-diff-command "diff"
   :positionals (list (make-positional :key :a :name "a" :required-p t)
                      (make-positional :key :b :name "b" :required-p t))
   :options (list (value-option "format" "Use one format for both files; otherwise use each extension."
                                :choices aitools.data:*document-format-choices*)
                  (integer-option "limit" 100 "Maximum operations returned.")
                  (tx-option))
   :handler (lambda (invocation)
              (%run-doc-flow
               ports invocation "diff"
               (list (positional-value invocation :a)
                     (positional-value invocation :b))
               (list (positional-value invocation :a)
                     (positional-value invocation :b))
               (list :limit (option-value invocation :limit)))))

(define-inspect-command "doc.get" "doc" '%doc-get-command
  aitools.data:*inspect-format-command-schemas*)
(define-inspect-command "doc.select" "doc" '%doc-select-command
  aitools.data:*inspect-format-command-schemas*)
(define-inspect-command "doc.diff" "doc" '%doc-diff-command
  aitools.data:*inspect-format-command-schemas*)
