;;;; packages/feature/inspect/src/presentation/archive-commands.lisp
;;;;
;;;; `archive list` and `archive read`.
(in-package #:aitools.inspect.presentation)

(defun %archive-list-command (ports)
  (make-command
   :name "list" :description (command-summary "archive.list" aitools.data:*inspect-archive-command-schemas*)
   :positionals (list (make-positional :key :path :name "path" :required-p t))
   :options (list (integer-option "limit" 200 "Maximum members returned.")
                  (tx-option))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:archive-list-flow ports
                     (positional-value invocation :path)
                     :limit (option-value invocation :limit)
                     (context-arguments invocation)))))

(defun %archive-read-command (ports)
  (make-command
   :name "read" :description (command-summary "archive.read" aitools.data:*inspect-archive-command-schemas*)
   :positionals (list (make-positional :key :path :name "path" :required-p t)
                      (make-positional :key :entry :name "entry" :required-p nil))
   :options (append (selector-options)
                    (list (value-option "as" "text or hex." :choices '("text" "hex") :default "text")
                          (integer-option "max-lines" 80 "Maximum lines returned.")
                          (tx-option)))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:archive-read-flow ports
                     (positional-value invocation :path)
                     (positional-value invocation :entry)
                     :as (option-value invocation :as)
                     :max-lines (option-value invocation :max-lines)
                     (append (selector-arguments invocation) (context-arguments invocation))))))

(define-inspect-command "archive.list" "archive" '%archive-list-command aitools.data:*inspect-archive-command-schemas*)
(define-inspect-command "archive.read" "archive" '%archive-read-command aitools.data:*inspect-archive-command-schemas*)
