(in-package #:aitools.integration.structure-test)

(defstruct liveness-reader-view-cache
  view
  replacements
  origins
  source-positions
  source-names
  cumulative-shift
  forms)

(defun %liveness-reader-view-cache-entry (text)
  (multiple-value-bind (view replacements)
      (%liveness-reader-view text)
    (let ((origins (make-hash-table :test (function equal)))
          (source-positions (make-hash-table :test (function equal)))
          (source-names (make-hash-table :test (function equal)))
          (forms (make-hash-table :test (function eql)))
          (replacements (coerce replacements 'vector))
          (shift 0))
      (loop for replacement across replacements
            for start = (first replacement)
            for end = (second replacement)
            for token = (third replacement)
            for view-end = (+ start shift 2 (length token))
            do (setf (gethash token origins) (fourth replacement)
                     (gethash token source-positions) start
                     (gethash token source-names) (fifth replacement))
               (setf (cdr (last replacement))
                     (list (incf shift (- (+ 2 (length token)) (- end start)))
                           view-end)))
      (labels ((cumulative-shift (position boundary-index)
                 (loop with low = 0
                       with high = (length replacements)
                       while (< low high)
                       for middle = (ash (+ low high) -1)
                       do (if (<= (nth boundary-index (aref replacements middle)) position)
                              (setf low (1+ middle))
                              (setf high middle))
                       finally (return (if (zerop low) 0
                                           (sixth (aref replacements (1- low))))))))
        (make-liveness-reader-view-cache
         :view view
         :replacements replacements
         :origins origins
         :source-positions source-positions
         :source-names source-names
         :cumulative-shift (function cumulative-shift)
         :forms forms)))))
