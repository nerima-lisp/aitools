(in-package #:aitools.kernel.domain)

;;; ------------------------------------------------------------- LCS table
;;;
;;; %EDIT-OP carries only a KIND and a COUNT. Positions are deliberately not
;;; tracked here -- %SCRIPT-REGIONS below derives exact, unambiguous
;;; positions by walking the merged script once, which is simpler than
;;; keeping backtracking and position bookkeeping consistent with each other.

(defstruct (%edit-op (:constructor %make-edit-op (kind count)) (:copier nil) (:predicate nil)) kind count)

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
