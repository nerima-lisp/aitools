;;;; packages/feature/search/src/domain/results.lisp
;;;;
;;;; One file's `search` outcome and its rendering. SEARCH-FILE runs on the
;;;; scan's worker threads and keeps only byte offsets; strings are made
;;;; later, by BUILD-BLOCKS and RENDER-MATCH, and only for the lines and
;;;; matches that fit under `--limit` (decode only output lines, so non-matching lines allocate nothing).
(in-package #:aitools.search.domain)

(defstruct (selected-line (:constructor %make-selected-line (number start end)) (:copier nil))
  (number 0 :type fixnum :read-only t)
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t))

(defstruct (found-match (:constructor %make-found-match (pattern-index line line-start start end groups))
                        (:copier nil))
  (pattern-index 0 :type fixnum :read-only t)
  (line 0 :type fixnum :read-only t)
  (line-start 0 :type fixnum :read-only t)
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  ;; (START . END) or NIL per explicit group, in group order.
  (groups #() :type simple-vector :read-only t))

(defstruct (file-outcome (:constructor %make-file-outcome (octets selected selected-count match-count matches))
                         (:copier nil))
  "OCTETS is the file without its BOM; offsets below index into it.
SELECTED lists the SELECTED-LINEs (blocks mode only), MATCHES the
FOUND-MATCHes (matches mode only); the counts are always exact."
  (octets nil :type (or null octets) :read-only t)
  (selected '() :type list :read-only t)
  (selected-count 0 :type fixnum :read-only t)
  (match-count 0 :type fixnum :read-only t)
  (matches '() :type list :read-only t))

(defun %match-groups (hit group-count)
  (let ((groups (make-array group-count :initial-element nil)))
    (dotimes (i group-count groups)
      (let ((start (hit-group-start hit (1+ i))))
        (when start
          (setf (svref groups i) (cons start (hit-group-end hit (1+ i)))))))))

(defun search-file (matcher octets mode keep)
  "Search OCTETS (a whole text file) and return its FILE-OUTCOME. MODE is
:BLOCKS, :MATCHES, :COUNT, :FILES, or :FILES-WITHOUT-MATCH; blocks and
matches keep the positions of at most KEEP lines or matches (no file can
contribute more than `--limit` of them), while the counts stay exact."
  (declare (type octets octets))
  (let ((octets (strip-bom octets)))
    (if (and (not (matcher-invert-p matcher)) (not (matcher-could-match-p matcher octets)))
        (%make-file-outcome nil '() 0 0 '())
        (ecase mode
          (:matches
           (let ((matches '()) (count 0) (programs (matcher-programs matcher)))
             (declare (type fixnum count))
             (flet ((on-match (index result line line-start)
                      (incf count)
                      (when (<= count keep)
                       (push (%make-found-match index line line-start
                                               (hit-start result) (hit-end result)
                                               (%match-groups result (program-group-count (svref programs index))))
                            matches))
                      nil))
               (declare (dynamic-extent #'on-match))
               (map-matches #'on-match matcher octets))
             (%make-file-outcome (and matches octets) '() 0 count (nreverse matches))))
          (:blocks
           (let ((selected '()) (count 0))
             (declare (type fixnum count))
             (flet ((on-line (line start end)
                      (incf count)
                      (when (<= count keep) (push (%make-selected-line line start end) selected))
                      nil))
               (declare (dynamic-extent #'on-line))
               (map-selected-lines #'on-line matcher octets))
             (%make-file-outcome (and selected octets) (nreverse selected) count 0 '())))
          ((:count :files :files-without-match)
           (let ((count 0))
             (declare (type fixnum count))
             (flet ((on-line (line start end)
                      (declare (ignore line start end))
                      (incf count)
                      nil))
               (declare (dynamic-extent #'on-line))
               (map-selected-lines #'on-line matcher octets))
             (%make-file-outcome nil '() count 0 '())))))))

;;; ------------------------------------------------------------ rendering

(defun %decode (octets start end)
  (values (decode-utf8 octets :start start :end end)))

(defun %lines-back (octets start count)
  "The start offset of the line COUNT lines before the one at START, and
how many lines back that actually is (fewer at the top of the file)."
  (let ((pos start) (moved 0))
    (loop while (and (< moved count) (plusp pos))
          do (setf pos (line-start-at octets (1- pos)))
             (incf moved))
    (values pos moved)))

(defun build-blocks (path octets selected before after)
  "`search` blocks for SELECTED (the SELECTED-LINEs to report, in order) with
BEFORE and AFTER lines of context. Blocks whose context touches or overlaps
merge into one. Returns (VALUES blocks characters), CHARACTERS being the
text returned, for `approx_tokens`."
  (let ((blocks '()) (characters 0)
        (first-line 0) (next-line 0) (next-pos 0) (lines '()) (match-lines '()))
    (labels ((close-block ()
               (when lines
                 (push (json-object-from-alist (list (cons "path" path) (cons "start_line" first-line)
                                          (cons "lines" (nreverse lines))
                                          (cons "match_lines" (nreverse match-lines))))
                       blocks)
                 (setf lines '() match-lines '())))
             (take-lines-through (last-line)
               ;; Append lines NEXT-LINE..LAST-LINE starting at NEXT-POS.
               (loop while (and (<= next-line last-line) (< next-pos (length octets)))
                     do (let ((text (%decode octets next-pos (line-content-end octets next-pos))))
                          (incf characters (length text))
                          (push text lines)
                          (setf next-pos (next-line-start octets next-pos))
                          (incf next-line)))))
      (dolist (line selected)
        (let ((number (selected-line-number line)))
          (multiple-value-bind (context-start moved) (%lines-back octets (selected-line-start line) before)
            (let ((from (- number moved)))
              (if (and lines (<= from next-line))
                  (take-lines-through number)
                  (progn
                    (close-block)
                    (setf first-line from next-line from next-pos context-start)
                    (take-lines-through number)))
              (push number match-lines)
              (take-lines-through (+ number after))))))
      (close-block))
    (values (nreverse blocks) characters)))

(defun render-match (path octets match programs with-pattern-index)
  "`search` `matches` item for MATCH. Returns (VALUES object characters)."
  (let* ((program (svref programs (found-match-pattern-index match)))
         (names (program-names program))
         (groups (found-match-groups match))
         (text (%decode octets (found-match-start match) (found-match-end match)))
         (characters (length text))
         (group-texts (map 'list (lambda (group)
                                   (if group
                                       (let ((value (%decode octets (car group) (cdr group))))
                                         (incf characters (length value))
                                         value)
                                       (json-null)))
                           groups))
         (named (loop for index from 1 below (length names)
                      for name = (svref names index)
                      when name collect (cons name (nth (1- index) group-texts)))))
    (values
     (json-object-from-alist
      (append (list (cons "path" path)
                    (cons "line" (found-match-line match))
                    (cons "col" (utf8-column octets (found-match-line-start match) (found-match-start match)))
                    (cons "text" text)
                    (cons "groups" group-texts))
              (when named (list (cons "named" (json-object-from-alist named))))
              (when with-pattern-index (list (cons "pattern_index" (found-match-pattern-index match))))))
     characters)))
