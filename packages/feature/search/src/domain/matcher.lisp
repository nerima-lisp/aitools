;;;; packages/feature/search/src/domain/matcher.lisp
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
;;;; Only `scan` is used: it runs a fresh Pike VM per call, so one compiled
;;;; matcher is safe to share between the scan's worker threads, whereas
;;;; `is-match-p` shares a lazily built DFA workspace between callers.
(in-package #:aitools.search.domain)

;;; A per-run cumulative budget. cl-regex-kit's step budget is per SCAN
;;; call, so it resets on every line; a pattern on the advanced executor that
;;; stays just under it on each of a file's (or a run's) many lines would run
;;; unbounded. *REGEX-RUN-DEADLINE* is an INTERNAL-REAL-TIME the flow sets for
;;; the whole run (shared by the worker threads through the value each binds
;;; on its own thread); MAP-MATCHES gives up once it is past, so the file is
;;; reported skipped `regex-limit` like a per-call exhaustion. The per-call
;;; reset itself is a cl-regex-kit matter, reported upstream, not patched here.
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

(defun build-matcher/k (patterns &key fixed ignore-case word line-regexp multiline invert
                                   on-built on-syntax-error)
  "Compile PATTERNS (strings) and call exactly one continuation: ON-BUILT
(matcher), or ON-SYNTAX-ERROR (index pattern message) for the first
pattern cl-regex-kit rejects."
  (declare (type function on-built on-syntax-error))
  (let ((programs '()))
    (loop for pattern in patterns
          for index from 0
          do (let* ((source (%pattern-source pattern fixed word line-regexp))
                    (regex (handler-case (%compile source ignore-case multiline)
                             (cl-regex-kit:cl-regex-kit-error (condition)
                               (return-from build-matcher/k
                                 (funcall on-syntax-error index pattern (princ-to-string condition)))))))
               (multiple-value-bind (literal fold) (%program-literal regex source ignore-case multiline)
                 (push (%make-program regex literal fold
                                      (coerce (cl-regex-kit:regex-capture-names regex) 'simple-vector)
                                      (cl-regex-kit:regex-group-count regex))
                       programs))))
    (funcall on-built (%make-matcher (coerce (nreverse programs) 'simple-vector)
                                     (and multiline t) (and invert t)))))

(defun matcher-could-match-p (matcher octets)
  "NIL when no pattern's required literal occurs in OCTETS, so no line of
the file can match: the per-file literal prefilter."
  (loop for program across (matcher-programs matcher)
        thereis (let ((literal (program-literal program)))
                  (or (null literal) (octets-find literal octets 0 :fold (program-fold program))))))

;;; ------------------------------------------------------------ matching

(defstruct (hit (:constructor %make-hit (starts ends)) (:copier nil))
  "One match: STARTS and ENDS hold the offsets (into the file's buffer) of
the whole match at index 0 and of each explicit group after it, NIL for a
group that did not participate."
  (starts #() :type simple-vector :read-only t)
  (ends #() :type simple-vector :read-only t))

(declaim (inline hit-start hit-end))
(defun hit-start (hit) (svref (hit-starts hit) 0))
(defun hit-end (hit) (svref (hit-ends hit) 0))

(defun hit-group-start (hit group) (svref (hit-starts hit) group))
(defun hit-group-end (hit group) (svref (hit-ends hit) group))

(defun %hit (regex result offset)
  "A HIT for cl-regex-kit's RESULT, its offsets moved by OFFSET."
  (let* ((count (1+ (cl-regex-kit:regex-group-count regex)))
         (starts (make-array count :initial-element nil))
         (ends (make-array count :initial-element nil)))
    (dotimes (group count)
      (let ((start (cl-regex-kit:match-group-start result group)))
        (when start
          (setf (svref starts group) (+ start offset)
                (svref ends group) (+ (cl-regex-kit:match-group-end result group) offset)))))
    (%make-hit starts ends)))

(defun scan-line (regex octets line-start line-end &optional (from line-start))
  "The first HIT of REGEX in the line [LINE-START, LINE-END) of OCTETS that
starts at or after FROM, or NIL. cl-regex-kit v2.1.1 bounds a byte-regex
`\\b`'s cost to the bytes around the position rather than the whole buffer,
so the scan runs in place: `:end` keeps the match inside the line, and a
line edge is what `^`, `$`, and `\\b` see there in any case."
  (declare (type octets octets) (type fixnum line-start line-end from))
  (let ((result (cl-regex-kit:scan regex octets :start from :end line-end)))
    (and result (%hit regex result 0))))

(defun %next-match (program octets pos multiline)
  "The leftmost HIT of PROGRAM starting at or after POS, or NIL."
  (declare (type octets octets) (type fixnum pos))
  (let ((regex (program-regex program))
        (literal (program-literal program))
        (fold (program-fold program))
        (length (length octets)))
    (if multiline
        (let ((result (and (or (null literal) (octets-find literal octets pos :fold fold))
                           (cl-regex-kit:scan regex octets :start pos))))
          (and result (%hit regex result 0)))
        (loop
          (when (> pos length) (return nil))
          (let ((candidate
                  (if literal
                      (or (octets-find literal octets pos :fold fold) (return nil))
                      (let ((result (or (cl-regex-kit:scan regex octets :start pos) (return nil))))
                        (if (<= (cl-regex-kit:match-end result)
                                (line-content-end octets (cl-regex-kit:match-start result)))
                            (return (%hit regex result 0))
                            (cl-regex-kit:match-start result))))))
            (let* ((line-start (line-start-at octets candidate))
                   (hit (scan-line regex octets line-start (line-content-end octets candidate)
                                   (max pos line-start))))
              (when hit (return hit))
              (setf pos (max (1+ pos) (next-line-start octets candidate)))))))))

(defun %line-at-p (octets position)
  "True when POSITION lies on a line: an empty file has none, and a final LF
ends the last line rather than starting an empty one."
  (declare (type octets octets) (type fixnum position))
  (let ((length (length octets)))
    (or (< position length)
        (and (= position length) (plusp length) (/= (aref octets (1- length)) 10)))))

;;; A match can sit at the buffer end (an empty match, or one on a last line
;;; with no LF), so the position after it must be able to pass that end;
;;; otherwise the walk finds the same match there forever.
(defun %after-empty-match (octets position)
  "The position to resume at after an empty match at POSITION."
  (declare (type octets octets) (type fixnum position))
  (if (< position (length octets)) (next-char-boundary octets position) (1+ (length octets))))

(defun %after-line (octets position)
  "The start of the line after the one holding POSITION, or past the buffer
end when that line is the last."
  (declare (type octets octets) (type fixnum position))
  (let ((lf (position 10 octets :start position)))
    (if lf (1+ lf) (1+ (length octets)))))

(defun map-matches (function matcher octets &key line-mode)
  "Call FUNCTION with (PATTERN-INDEX HIT LINE LINE-START) for each
match in OCTETS, leftmost first across all patterns (a tie goes to the
lower pattern index), without overlaps. LINE is the 1-based line of the
match start and LINE-START its offset. With LINE-MODE only the first match
of each line is reported (the rest of a multi-line match's lines are
skipped too). A pattern the bounded advanced executor gives up on signals
CL-REGEX-KIT:ADVANCED-REGEX-LIMIT-ERROR."
  (declare (type function function) (type octets octets))
  (let* ((programs (matcher-programs matcher))
         (count (length programs))
         (multiline (matcher-multiline-p matcher))
         (cache (make-array count :initial-element nil))
         (pos 0)
         (skip-empty-at -1)
         (line 1)
         (line-pos 0))
    (declare (type fixnum pos skip-empty-at line line-pos))
    (loop
      (when (%regex-deadline-passed-p) (error 'regex-budget-exhausted))
      (let ((best nil) (best-index -1))
        (dotimes (i count)
          (let ((cached (svref cache i)))
            (when (or (null cached)
                      (and (not (eq cached :none)) (< (hit-start cached) pos)))
              (setf cached (or (%next-match (svref programs i) octets pos multiline) :none)
                    (svref cache i) cached))
            (when (and (not (eq cached :none))
                       (or (null best) (< (hit-start cached) (hit-start best))))
              (setf best cached best-index i))))
        (when (or (null best) (not (%line-at-p octets (hit-start best))))
          (return))
        (let ((start (hit-start best))
              (end (hit-end best)))
          (declare (type fixnum start end))
          (if (and (= start end) (= start skip-empty-at) (not line-mode))
              (setf pos (%after-empty-match octets start))
              (progn
                (incf line (count-newlines octets line-pos start))
                (setf line-pos start)
                (funcall function best-index best line (line-start-at octets start))
                (cond
                  (line-mode (setf pos (%after-line octets (if (> end start) (1- end) start))))
                  ((> end start) (setf pos end skip-empty-at end))
                  (t (setf pos (%after-empty-match octets start)))))))
        (when (> pos (length octets)) (return))))))

(defun map-selected-lines (function matcher octets)
  "Call FUNCTION with (LINE CONTENT-START CONTENT-END) for each line `search`
selects, in order: lines holding a match (every line a `--multiline`
match spans), or with `--invert` the lines holding none."
  (declare (type function function) (type octets octets))
  (let ((length (length octets))
        (walk-line 1)
        (walk-pos 0)
        (invert (matcher-invert-p matcher)))
    (declare (type fixnum length walk-line walk-pos))
    (flet ((emit-until (last-line)
             ;; Emit lines from WALK-LINE through LAST-LINE, advancing the
             ;; cursor.
             (loop while (and (<= walk-line last-line) (< walk-pos length))
                   do (funcall function walk-line walk-pos (line-content-end octets walk-pos))
                      (setf walk-pos (next-line-start octets walk-pos))
                      (incf walk-line)))
           (skip-until (last-line)
             (loop while (and (<= walk-line last-line) (< walk-pos length))
                   do (setf walk-pos (next-line-start octets walk-pos))
                      (incf walk-line))))
      (flet ((on-match (index result line line-start)
               (declare (ignore index line-start))
               (let* ((start (hit-start result))
                      (end (hit-end result))
                      (last (+ line (count-newlines octets start (if (> end start) (1- end) start)))))
                 (if invert
                     (progn (emit-until (1- line)) (skip-until last))
                     (progn (skip-until (1- line)) (emit-until last))))
               nil))
        (declare (dynamic-extent #'on-match))
        (map-matches #'on-match matcher octets :line-mode t))
      (when invert (emit-until most-positive-fixnum)))))

(defun call-with-regex-budget/k (thunk &key on-exhausted)
  "Return THUNK's values, or ON-EXHAUSTED's when a pattern runs out of its
budget: cl-regex-kit's per-call step budget on the bounded advanced executor
(backreferences, lookaround, ...), or the per-run cumulative budget
*REGEX-RUN-DEADLINE*. `search` reports such a file as skipped rather than
failing the whole command."
  (declare (type function thunk on-exhausted))
  (handler-case (return-from call-with-regex-budget/k (funcall thunk))
    ((or cl-regex-kit:advanced-regex-limit-error regex-budget-exhausted) () nil))
  (funcall on-exhausted))
