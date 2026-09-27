;;;; packages/core/kernel/src/domain/guard.lisp
;;;;
;;;; The two guards (docs/src/reference/commands.md), `--expect-hash` and `--expect-count`, as
;;;; parsing plus the selector-driven half of "when is this guard required".
;;;; The command-specific half (`write --overwrite`, `move`/`copy`/`link`
;;;; replacing an existing target, `table set`) has no selector to consult and
;;;; is each command's own presentation-layer concern.
(in-package #:aitools.kernel.domain)

(defstruct (expect-hash-entry
            (:constructor make-expect-hash-entry (path hash))
            (:copier nil))
  "PATH is NIL for the single-target form `--expect-hash <hash>`, and the
left side of `=` for the repeated multi-target form `--expect-hash
<path>=<hash>`."
  (path nil :type (or null string) :read-only t)
  (hash nil :type simple-string :read-only t))

(defun parse-expect-hash-argument (argument)
  "Parse one `--expect-hash` occurrence. `path=hash` splits on the FIRST `=`,
since a hash is hex and never contains one; an argument with no `=` is the
single-target form and PATH is NIL. Signals a SIMPLE-ERROR on an empty hash."
  (let* ((equals (position #\= argument))
         (path (and equals (subseq argument 0 equals)))
         (hash (if equals (subseq argument (1+ equals)) argument)))
    (when (zerop (length hash))
      (error "--expect-hash requires a non-empty hash: ~S" argument))
    (when (and path (zerop (length path)))
      (error "--expect-hash path=hash form requires a non-empty path: ~S" argument))
    (make-expect-hash-entry path hash)))

(defun guard-required-p (guard selector)
  "True when GUARD (:EXPECT-HASH or :EXPECT-COUNT) is required given
SELECTOR is the selector a write command was invoked
with (or NIL, for a write with no selector at all).

:EXPECT-HASH is required for a position-based selector (`--range`,
`--symbol`), since writing at a stale line number is the accident this guard
exists to catch. :EXPECT-COUNT is required for `--match`, the selector whose
match set can silently grow or shrink between when an agent inspected the
file and when it writes."
  (ecase guard
    (:expect-hash (and selector (eq (selector-basis selector) :position)))
    (:expect-count (and selector (eq (selector-kind selector) :match)))))
