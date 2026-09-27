;;;; packages/feature/inspect/src/presentation/snapshot-commands.lisp
;;;;
;;;; `snapshot create` and `snapshot diff`. They run against the disk only
;;;; (docs/src/reference/transactions.md), so they take no `--tx`.
(in-package #:aitools.inspect.presentation)

(defun %snapshot-context-arguments (invocation)
  (list :root (option-value invocation :root)
        :lock-timeout (option-value invocation :lock-timeout)))

(defun %snapshot-create-command (ports)
  (make-command
   :name "create" :description (command-summary "snapshot.create" aitools.data:*inspect-archive-command-schemas*)
   :options (list (make-option :name "glob" :kind :value :multiple-p t :value-name "GLOB"
                               :description "Only paths matching this glob (repeatable).")
                  (value-option "lang" "Only files of this language." :value-name "LANG")
                  (flag-option "no-ignore" "Include ignored files.")
                  (value-option "skip-larger-than" "Leave out larger files." :value-name "SIZE")
                  (value-option "newer" "A path or a duration." :value-name "PATH|DURATION"))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:snapshot-create-flow ports
                     :glob (option-value invocation :glob)
                     :lang (option-value invocation :lang)
                     :no-ignore (option-value invocation :no-ignore)
                     :skip-larger-than (option-value invocation :skip-larger-than)
                     :newer (option-value invocation :newer)
                     (%snapshot-context-arguments invocation)))))

(defun %snapshot-diff-command (ports)
  (make-command
   :name "diff" :description (command-summary "snapshot.diff" aitools.data:*inspect-archive-command-schemas*)
   :positionals (list (make-positional :key :id :name "snapshot_id" :required-p t))
   :options (list (integer-option "limit" 100 "Maximum paths per list."))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:snapshot-diff-flow ports
                     (positional-value invocation :id)
                     :limit (option-value invocation :limit)
                     (%snapshot-context-arguments invocation)))))

(define-inspect-command "snapshot.create" "snapshot" '%snapshot-create-command aitools.data:*inspect-archive-command-schemas*)
(define-inspect-command "snapshot.diff" "snapshot" '%snapshot-diff-command aitools.data:*inspect-archive-command-schemas*)
