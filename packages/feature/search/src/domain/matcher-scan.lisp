(in-package #:aitools.search.domain)

(defstruct (hit (:constructor %make-hit (starts ends)) (:copier nil) (:predicate nil))
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

(defun scan-line (regex octets from line-end)
  "The HIT of REGEX in OCTETS with the earliest start at or after FROM;
REGEX alternative order breaks ties. LINE-END is the exclusive upper
bound for matched bytes, but an empty HIT at LINE-END is allowed. Offsets
remain relative to OCTETS."
  (declare (type octets octets) (type fixnum from line-end))
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
                   (hit (scan-line regex octets (max pos line-start) (line-content-end octets candidate))))
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
