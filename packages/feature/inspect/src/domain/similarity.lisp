;;;; packages/feature/inspect/src/domain/similarity.lisp
;;;;
;;;; Ranking for `candidates` (a missing file, entry, or selector match comes
;;;; back with its nearest neighbours; see docs/src/reference/errors.md).
;;;; cl-cli's Levenshtein is not exported, so this is a bounded
;;;; one: inputs longer than +SIMILARITY-MAX-CHARS+ are compared by their
;;;; prefix, keeping the cost per comparison fixed.
(in-package #:aitools.inspect.domain)

(defconstant +similarity-max-chars+ 256)

(defun edit-distance (a b)
  "Levenshtein distance between A and B (each cut to +SIMILARITY-MAX-CHARS+)."
  (let* ((a (if (> (length a) +similarity-max-chars+) (subseq a 0 +similarity-max-chars+) a))
         (b (if (> (length b) +similarity-max-chars+) (subseq b 0 +similarity-max-chars+) b))
         (n (length b))
         (previous (make-array (1+ n) :element-type 'fixnum))
         (current (make-array (1+ n) :element-type 'fixnum)))
    (declare (type string a b) (type fixnum n))
    (dotimes (j (1+ n)) (setf (aref previous j) j))
    (loop for i from 1 to (length a)
          do (setf (aref current 0) i)
             (loop for j from 1 to n
                   do (setf (aref current j)
                            (min (1+ (aref previous j))
                                 (1+ (aref current (1- j)))
                                 (+ (aref previous (1- j))
                                    (if (char= (char a (1- i)) (char b (1- j))) 0 1)))))
             (rotatef previous current))
    (aref previous n)))

(defun rank-similar (target items &key (key #'identity) (count 3) max-distance)
  "Up to COUNT of ITEMS nearest to TARGET by EDIT-DISTANCE of (KEY item),
nearest first, ties in ITEMS order. With MAX-DISTANCE, farther items are
dropped."
  (let ((scored (loop for item in items
                      for index from 0
                      for distance = (edit-distance target (funcall key item))
                      when (or (null max-distance) (<= distance max-distance))
                        collect (list distance index item))))
    (mapcar #'third
            (subseq (sort scored (lambda (x y)
                                   (or (< (first x) (first y))
                                       (and (= (first x) (first y)) (< (second x) (second y))))))
                    0 (min count (length scored))))))
