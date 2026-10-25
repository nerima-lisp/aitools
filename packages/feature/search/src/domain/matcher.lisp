;;;;
;;;; The `search` matcher over one file's bytes: cl-regex-kit byte regexes,
;;;; no allocation for non-matching lines, and a per-file literal prefilter.
;;;;
;;;; Each pattern compiles to a cl-regex-kit byte regex with `:multi-line`
;;;; (so `^`/`$` are line anchors), `:crlf` (so `$` also sits before CR LF),
;;;; and, unless `--multiline`, `:never-newline`, so no match can cross a line
;;;; end. The engine is then pointed only at candidate lines: a pattern with a
;;;; required literal (`regex-required-literals`) finds candidates by a plain
;;;; byte search for that literal, and the regex runs on the candidate's line
;;;; alone; a pattern without one lets the engine find the next match and
;;;; re-runs it on that match's line with the CR excluded. A line holding no
;;;; candidate is never handed to the engine and never decoded.
;;;;
;;;; `scan` creates a fresh Pike VM per call, so one compiled
;;;; matcher is safe to share between the scan's worker threads, whereas
;;;; `is-match-p` shares a lazily built DFA workspace between callers.
(in-package #:aitools.search.domain)

;;; A per-run cumulative budget. cl-regex-kit's step budget is per SCAN
;;; call, so it resets on every line; a pattern on the advanced executor that
;;; stays just under it on each of a file's (or a run's) many lines would run
;;; unbounded. *REGEX-RUN-DEADLINE* is an INTERNAL-REAL-TIME the flow sets for
;;; the whole run (shared by the worker threads through the value each binds
;;; on its own thread); MAP-MATCHES gives up once it is past, so the file is
;;; reported skipped `regex-limit`, the same result as a per-call exhaustion.
(defvar *regex-run-deadline* nil
  "INTERNAL-REAL-TIME after which MAP-MATCHES aborts with
REGEX-BUDGET-EXHAUSTED, or NIL for no cap.")

(define-condition regex-budget-exhausted (error) ()
  (:documentation "Signalled when matching passes *REGEX-RUN-DEADLINE*."))

(declaim (inline %regex-deadline-passed-p))
(defun %regex-deadline-passed-p ()
  (and *regex-run-deadline* (> (get-internal-real-time) *regex-run-deadline*)))

(defstruct (program (:constructor %make-program (regex literal fold names group-count)) (:copier nil))
  (regex nil :read-only t)
  ;; A byte sequence every match contains, or NIL.
  (literal nil :type (or null octets) :read-only t)
  ;; LITERAL is lowercase and compared ASCII-case-insensitively.
  (fold nil :type boolean :read-only t)
  ;; Capture names indexed by group number (index 0 is the whole match).
  (names #() :type simple-vector :read-only t)
  (group-count 0 :type fixnum :read-only t))

(defstruct (matcher (:constructor %make-matcher (programs multiline-p invert-p)) (:copier nil))
  (programs #() :type simple-vector :read-only t)
  (multiline-p nil :type boolean :read-only t)
  (invert-p nil :type boolean :read-only t))

(defun %literal-octets (literal)
  "A required literal from a byte regex: its char codes are the octets."
  (map 'octets #'char-code literal))

(defun %fold-safe-byte-p (byte)
  "True for bytes an ASCII case-insensitive comparison handles exactly. Only
ASCII qualifies, and `k` and `s` do not: Unicode simple case folding also
maps KELVIN SIGN to `k` and LONG S to `s`."
  (and (< byte 128) (not (member byte '(75 83 107 115)))))

(defun %fold-literal (literals)
  "The longest run of fold-safe bytes in any of LITERALS, lowercased, or NIL."
  (let ((best nil))
    (dolist (literal literals best)
      (let ((octets (%literal-octets literal)) (start nil))
        (flet ((close-run (end)
                 (when (and start (> end start) (or (null best) (> (- end start) (length best))))
                   (setf best (map 'octets (lambda (byte) (if (<= 65 byte 90) (+ byte 32) byte))
                                   (subseq octets start end))))
                 (setf start nil)))
          (loop for i from 0 below (length octets)
                do (if (%fold-safe-byte-p (aref octets i))
                       (unless start (setf start i))
                       (close-run i))
                finally (close-run (length octets))))))))

(defun %pattern-source (pattern fixed word line-regexp)
  (let ((source (if fixed (cl-regex-kit:escape pattern) pattern)))
    (cond (line-regexp (format nil "^(?:~A)$" source))
          (word (format nil "\\b(?:~A)\\b" source))
          (t source))))

(defun %compile (source ignore-case multiline)
  (cl-regex-kit:compile-byte-regex source :case-insensitive ignore-case :multi-line t :crlf t
                                          :never-newline (not multiline)))

(defun %program-literal (regex source ignore-case multiline)
  "(VALUES literal fold) for a compiled pattern. Under `--ignore-case` the
kit derives no literal, so the literals of the same pattern compiled
case-sensitively are searched for ASCII-case-insensitively instead."
  (let ((literals (cl-regex-kit:regex-required-literals regex)))
    (cond
      (literals (values (%literal-octets (first literals)) nil))
      (ignore-case
       (let* ((sensitive (handler-case (%compile source nil multiline)
                           (cl-regex-kit:cl-regex-kit-error () nil)))
              (folded (and sensitive (%fold-literal (cl-regex-kit:regex-required-literals sensitive)))))
         (values folded (and folded t))))
      (t (values nil nil)))))

(aitools.kernel.domain:define-flow
    (build-matcher/k patterns &key fixed ignore-case word line-regexp multiline invert
                     on-built on-syntax-error)
  "Compile PATTERNS (strings) and call exactly one continuation: ON-BUILT
(matcher), or ON-SYNTAX-ERROR (index pattern message) for the first
pattern cl-regex-kit rejects."
  (let ((programs '())
        (failure-index nil)
        (failure-pattern nil)
        (failure-message nil)
        (failed-p nil))
    (loop for pattern in patterns
          for index from 0
          until failed-p
          do (let* ((source (%pattern-source pattern fixed word line-regexp))
                    (regex (handler-case (%compile source ignore-case multiline)
                             (cl-regex-kit:cl-regex-kit-error (condition)
                               (setf failed-p t
                                     failure-index index
                                     failure-pattern pattern
                                     failure-message (princ-to-string condition))
                               nil))))
               (unless failed-p
                 (multiple-value-bind (literal fold) (%program-literal regex source ignore-case multiline)
                   (push (%make-program regex literal fold
                                        (coerce (cl-regex-kit:regex-capture-names regex) 'simple-vector)
                                        (cl-regex-kit:regex-group-count regex))
                         programs)))))
    (if failed-p
        (aitools.kernel.domain:flow-continue
         :on-syntax-error failure-index failure-pattern failure-message)
        (aitools.kernel.domain:flow-continue
         :on-built (%make-matcher (coerce (nreverse programs) 'simple-vector)
                                  (and multiline t) (and invert t))))))

(defun matcher-could-match-p (matcher octets)
  "NIL when no pattern's required literal occurs in OCTETS, so no line of
the file can match: the per-file literal prefilter."
  (loop for program across (matcher-programs matcher)
        thereis (let ((literal (program-literal program)))
                  (or (null literal) (octets-find literal octets 0 :fold (program-fold program))))))

(aitools.kernel.domain:define-flow
    (call-with-regex-budget/k thunk &key on-result on-exhausted)
  "Call ON-RESULT with THUNK's value. Call ON-EXHAUSTED when cl-regex-kit
exhausts its per-call step budget for advanced regex features such as
backreferences and lookaround, or when *REGEX-RUN-DEADLINE* expires. The
search flow reports the affected file as skipped."
  (let ((outcome
          (handler-case (list :result (funcall thunk))
            ((or cl-regex-kit:advanced-regex-limit-error regex-budget-exhausted) ()
              (list :exhausted)))))
    (ecase (first outcome)
      (:result (aitools.kernel.domain:flow-continue :on-result (second outcome)))
      (:exhausted (aitools.kernel.domain:flow-continue :on-exhausted)))))
