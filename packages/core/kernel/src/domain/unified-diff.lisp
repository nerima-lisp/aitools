;;;; packages/core/kernel/src/domain/unified-diff.lisp
;;;;
;;;; No kit in the org generates or applies unified diffs,
;;;; so `diff` and `apply` both build on this file. The LCS
;;;; step is a classic Wagner-Fischer dynamic-programming table (a
;;;; value-returning computation with no continuation-passing boundary; see
;;;; the CPS audit in docs/src/reference/architecture.md), bounded by a cap on
;;;; the table's cell count -- the same shape paredit-cli's diff.rs uses to
;;;; keep a diff over an unrelated pair of large files cheap: past the limit,
;;;; GENERATE-DIFF-HUNKS falls back to one hunk that replaces the whole file
;;;; instead of allocating an unbounded table.
;;;;
;;;; Parsing and applying a diff are in unified-diff-patch.lisp.
(in-package #:aitools.kernel.domain)

;;; ------------------------------------------------------------- diff lines

(defstruct (diff-line
            (:constructor make-diff-line (op text &key no-newline))
            (:copier nil))
  "OP is :CONTEXT (unchanged, in both files), :DELETE (only in the old file),
or :INSERT (only in the new file). TEXT never includes a trailing newline.
NO-NEWLINE is true when this occurrence of TEXT is the last line of its file
and that file has no trailing newline -- the condition unified diff spells
out with a following `\\\\ No newline at end of file` line."
  (op nil :type (member :context :delete :insert) :read-only t)
  (text nil :type string :read-only t)
  (no-newline nil :type boolean :read-only t))

(defstruct (diff-hunk
            (:constructor make-diff-hunk (old-start old-count new-start new-count lines))
            (:copier nil))
  "OLD-START/NEW-START are 1-based line numbers, except that a zero count
uses the GNU diff convention of naming the line the insertion/deletion is
adjacent to (0 for \"before line 1\")."
  (old-start nil :type integer :read-only t)
  (old-count nil :type (integer 0) :read-only t)
  (new-start nil :type integer :read-only t)
  (new-count nil :type (integer 0) :read-only t)
  (lines nil :type list :read-only t))

;;; ------------------------------------------------------------- LCS table
;;;
;;; %EDIT-OP carries only a KIND and a COUNT. Positions are deliberately not
;;; tracked here -- %SCRIPT-REGIONS below derives exact, unambiguous
;;; positions by walking the merged script once, which is simpler than
;;; keeping backtracking and position bookkeeping consistent with each other.

(defstruct (%edit-op (:constructor %make-edit-op (kind count))) kind count)

(defparameter *default-max-lcs-cells* 4000000
  "Bounds the cost of GENERATE-DIFF-HUNKS: a (LENGTH A + 1) * (LENGTH B + 1)
table past this many cells (roughly a 2000-line file against another) is not
built; the two files are treated as unrelated and diffed as one wholesale
replacement instead.")

(defun %lcs-table (a b)
  "The classic Wagner-Fischer (N+1)x(M+1) table of longest-common-subsequence
lengths for vectors A and B, comparing elements with EQUAL."
  (let* ((n (length a)) (m (length b))
         (table (make-array (list (1+ n) (1+ m)) :element-type 'fixnum :initial-element 0)))
    (loop for i from 1 to n
          do (loop for j from 1 to m
                   do (setf (aref table i j)
                            (if (equal (aref a (1- i)) (aref b (1- j)))
                                (1+ (aref table (1- i) (1- j)))
                                (max (aref table (1- i) j) (aref table i (1- j)))))))
    table))

(defun %edit-script-from-lcs-table (a b table)
  "Backtrack TABLE from (LENGTH A, LENGTH B) to (0, 0), returning a
chronological (earliest-first) list of merged %EDIT-OPs. Preferring a delete
over an insert on a tie keeps behavior deterministic between runs."
  (let ((i (length a)) (j (length b)) (ops nil))
    (loop while (and (> i 0) (> j 0))
          do (cond
               ((equal (aref a (1- i)) (aref b (1- j)))
                (push (%make-edit-op :equal 1) ops) (decf i) (decf j))
               ((>= (aref table (1- i) j) (aref table i (1- j)))
                (push (%make-edit-op :delete 1) ops) (decf i))
               (t
                (push (%make-edit-op :insert 1) ops) (decf j))))
    (loop while (> i 0) do (push (%make-edit-op :delete 1) ops) (decf i))
    (loop while (> j 0) do (push (%make-edit-op :insert 1) ops) (decf j))
    (%merge-adjacent-edit-ops ops)))

(defun %merge-adjacent-edit-ops (ops)
  "Collapse consecutive single-step %EDIT-OPs of the same kind into one run.
OPS must already be earliest-first; each PUSH above prepends the op for the
position just backed into, so by the time the backtrack reaches (0, 0), OPS
is already in chronological (earliest-first) order without a final reverse."
  (let (result)
    (dolist (op ops (nreverse result))
      (let ((previous (first result)))
        (if (and previous (eq (%edit-op-kind previous) (%edit-op-kind op)))
            (setf (%edit-op-count previous) (+ (%edit-op-count previous) 1))
            (push op result))))))

(defun %edit-script (a b &key (max-cells *default-max-lcs-cells*))
  "Return a chronological list of %EDIT-OP runs turning vector A into vector
B, comparing elements with EQUAL. Returns :REPLACE-ALL when the LCS table
would exceed MAX-CELLS cells."
  (if (> (* (1+ (length a)) (1+ (length b))) max-cells)
      :replace-all
      (%edit-script-from-lcs-table a b (%lcs-table a b))))

;;; ------------------------------------------------------- hunk assembly

(defun %script-regions (script)
  "Walk SCRIPT (a list of %EDIT-OP runs) once, returning a list of plists
(:KIND :A-START :A-END :B-START :B-END), the exact 0-based half-open ranges
each run covers in the old and new sequences."
  (let ((a-pos 0) (b-pos 0))
    (loop for op in script
          collect (let ((kind (%edit-op-kind op)) (count (%edit-op-count op))
                        (a-start 0) (b-start 0))
                    (setf a-start a-pos b-start b-pos)
                    (ecase kind
                      (:equal (incf a-pos count) (incf b-pos count))
                      (:delete (incf a-pos count))
                      (:insert (incf b-pos count)))
                    (list :kind kind :a-start a-start :a-end a-pos :b-start b-start :b-end b-pos)))))

(defun %script-blocks (regions)
  "Merge REGIONS into (:EQUAL a-start a-end b-start b-end) and (:CHANGE
a-start a-end b-start b-end) blocks, collapsing every run of consecutive
non-equal regions -- whatever mix of deletes and inserts Myers' backtrack
produced -- into a single :CHANGE block. A :CHANGE block's lines are always
rendered as \"every deleted line, then every inserted line\" (see
%RENDER-BLOCK-LINES), so the internal delete/insert order within the merged
run does not need to be preserved."
  (let (blocks pending)
    (dolist (region regions)
      (if (eq (getf region :kind) :equal)
          (progn
            (when pending (push pending blocks) (setf pending nil))
            (push (list :equal (getf region :a-start) (getf region :a-end)
                        (getf region :b-start) (getf region :b-end))
                  blocks))
          (setf pending
                (if pending
                    (list :change (second pending) (getf region :a-end)
                          (fourth pending) (getf region :b-end))
                    (list :change (getf region :a-start) (getf region :a-end)
                          (getf region :b-start) (getf region :b-end))))))
    (when pending (push pending blocks))
    (nreverse blocks)))

(defun %group-blocks-into-hunks (blocks context)
  "Group BLOCKS into hunks: trim leading/trailing :EQUAL blocks to CONTEXT
lines, and split the group whenever a middle :EQUAL block is longer than
2*CONTEXT (mirroring `diff -u`'s own hunk-merging distance). Returns a list
of block-lists, each block-list a single hunk's worth of blocks in order."
  (let ((n (length blocks)) (groups nil) (current nil))
    (loop for block in blocks
          for i from 0
          do (if (eq (first block) :change)
                 (push block current)
                 (destructuring-bind (tag a-start a-end b-start b-end) block
                   (declare (ignore tag))
                   (let ((len (- a-end a-start)))
                     (cond
                       ((and (zerop i) (< i (1- n)))
                        (push (if (> len context)
                                  (list :equal (- a-end context) a-end (- b-end context) b-end)
                                  block)
                              current))
                       ((= i (1- n))
                        (push (if (> len context)
                                  (list :equal a-start (+ a-start context) b-start (+ b-start context))
                                  block)
                              current)
                        (push (nreverse current) groups)
                        (setf current nil))
                       ((> len (* 2 context))
                        (push (list :equal a-start (+ a-start context) b-start (+ b-start context)) current)
                        (push (nreverse current) groups)
                        (setf current (list (list :equal (- a-end context) a-end (- b-end context) b-end))))
                       (t (push block current))))))
          finally (when current (push (nreverse current) groups)))
    (nreverse groups)))

(defun %render-block-lines (a b block)
  (destructuring-bind (tag a-start a-end b-start b-end) block
    (ecase tag
      (:equal (loop for i from a-start below a-end collect (make-diff-line :context (aref a i))))
      (:change (append (loop for i from a-start below a-end collect (make-diff-line :delete (aref a i)))
                       (loop for i from b-start below b-end collect (make-diff-line :insert (aref b i))))))))

(defun %hunk-position (start count)
  (if (zerop count) start (1+ start)))

(defun %hunk-from-group (a b group)
  (let* ((first-block (first group)) (last-block (car (last group)))
         (a-start (second first-block)) (a-end (third last-block))
         (b-start (fourth first-block)) (b-end (fifth last-block)))
    (make-diff-hunk (%hunk-position a-start (- a-end a-start)) (- a-end a-start)
                    (%hunk-position b-start (- b-end b-start)) (- b-end b-start)
                    (loop for block in group append (%render-block-lines a b block)))))

(defun generate-diff-hunks (lines-a lines-b &key (context 3)
                             (max-cells *default-max-lcs-cells*)
                             (final-newline-a t) (final-newline-b t))
  "Diff two files given as vectors (or lists) of line strings without
trailing newlines, returning a list of DIFF-HUNKs in file order. CONTEXT is
the number of unchanged lines kept on each side of a change, as `diff -u`'s
own default. FINAL-NEWLINE-A/-B (default true) mark the last DIFF-LINE that
touches the corresponding file's true last line with NO-NEWLINE when false."
  (let* ((a (coerce lines-a 'simple-vector)) (b (coerce lines-b 'simple-vector))
         (script (%edit-script a b :max-cells max-cells)))
    (when (eq script :replace-all)
      (setf script (list (%make-edit-op :delete (length a)) (%make-edit-op :insert (length b)))))
    (let ((blocks (%script-blocks (%script-regions script))))
      (if (notany (lambda (block) (eq (first block) :change)) blocks)
          nil
          (let ((hunks (mapcar (lambda (group) (%hunk-from-group a b group))
                               (%group-blocks-into-hunks blocks context))))
            (%attach-no-newline-markers hunks (length a) (length b) final-newline-a final-newline-b))))))

(defun %attach-no-newline-markers (hunks a-length b-length final-newline-a final-newline-b)
  "Return HUNKS with the last hunk replaced by a copy whose DIFF-LINEs
covering the true last line of A and/or B carry NO-NEWLINE, when that file
lacks a trailing newline. A file whose last line falls outside every hunk
(unchanged and more than CONTEXT lines from any edit) cannot express this in
unified-diff form at all; APPLY-HUNKS/K leaves such a target's own trailing
newline untouched, which is the correct result."
  (if (or (null hunks) (and final-newline-a final-newline-b))
      hunks
      (let* ((last-hunk (car (last hunks)))
             (old-line (diff-hunk-old-start last-hunk))
             (new-line (diff-hunk-new-start last-hunk))
             (new-lines
               (mapcar (lambda (line)
                         (let* ((has-old (member (diff-line-op line) '(:context :delete)))
                                (has-new (member (diff-line-op line) '(:context :insert)))
                                (mark-a (and has-old (not final-newline-a) (= old-line a-length)))
                                (mark-b (and has-new (not final-newline-b) (= new-line b-length)))
                                (marked (if (or mark-a mark-b)
                                            (make-diff-line (diff-line-op line) (diff-line-text line)
                                                            :no-newline t)
                                            line)))
                           (when has-old (incf old-line))
                           (when has-new (incf new-line))
                           marked))
                       (diff-hunk-lines last-hunk))))
        (append (butlast hunks)
                (list (make-diff-hunk (diff-hunk-old-start last-hunk) (diff-hunk-old-count last-hunk)
                                      (diff-hunk-new-start last-hunk) (diff-hunk-new-count last-hunk)
                                      new-lines))))))

;;; ------------------------------------------------------------- rendering

(defun %hunk-header (hunk)
  (format nil "@@ -~D,~D +~D,~D @@~%"
          (diff-hunk-old-start hunk) (diff-hunk-old-count hunk)
          (diff-hunk-new-start hunk) (diff-hunk-new-count hunk)))

(defun %render-diff-line (line stream)
  (write-char (ecase (diff-line-op line) (:context #\Space) (:delete #\-) (:insert #\+)) stream)
  (write-string (diff-line-text line) stream)
  (write-char #\Newline stream)
  (when (diff-line-no-newline line)
    (write-string "\\ No newline at end of file" stream)
    (write-char #\Newline stream)))

(defun render-hunks (hunks &key path-a path-b)
  "HUNKS (DIFF-HUNKs) as unified-diff text: each hunk's `@@` header and body
lines, one line per DIFF-LINE with its `\\ No newline at end of file` marker.
With PATH-A given, precede the hunks with verbatim `--- PATH-A` / `+++ PATH-B`
file headers -- the caller supplies the exact header paths, so no `a/`/`b/`
prefix is added here (contrast GENERATE-UNIFIED-DIFF, which prefixes them)."
  (with-output-to-string (out)
    (when path-a
      (format out "--- ~A~%+++ ~A~%" path-a path-b))
    (dolist (hunk hunks)
      (write-string (%hunk-header hunk) out)
      (dolist (line (diff-hunk-lines hunk))
        (%render-diff-line line out)))))

(defun generate-unified-diff (lines-a lines-b &key path-a path-b (context 3)
                               (final-newline-a t) (final-newline-b t)
                               (max-cells *default-max-lcs-cells*))
  "Render a full unified diff (file headers plus every hunk) between LINES-A
and LINES-B. Returns an empty string when the files are identical."
  (let ((hunks (generate-diff-hunks lines-a lines-b :context context :max-cells max-cells
                                    :final-newline-a final-newline-a :final-newline-b final-newline-b)))
    (if (null hunks)
        ""
        (with-output-to-string (out)
          (format out "--- a/~A~%+++ b/~A~%" path-a path-b)
          (dolist (hunk hunks)
            (write-string (%hunk-header hunk) out)
            (dolist (line (diff-hunk-lines hunk))
              (%render-diff-line line out)))))))
