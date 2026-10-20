(in-package #:aitools.edit.domain)

(defun %version-parts (string)
  (loop with position = 0
        and length = (length string)
        while (< position length)
        collect (let* ((digit (%ascii-digit-p (char string position)))
                       (end
                        (or
                         (position-if
                          (lambda (c)
                            (if digit
                                (not (%ascii-digit-p c))
                                (%ascii-digit-p c)))
                          string
                          :start
                          position)
                         length)))
                  (prog1
                    (if digit
                        (parse-integer string :start position :end end)
                        (subseq string position end))
                    (setf position end)))))

(defun version< (a b)
  "True when version string A sorts before B: digit runs compare as numbers
(1.9 < 1.10), other runs as strings, a number before a non-number."
  (loop for xs = (%version-parts a) then (rest xs)
        for ys = (%version-parts b) then (rest ys)
        do (cond
             ((null ys) (return nil))
             ((null xs) (return t))
             (t
              (let ((x (first xs))
                    (y (first ys)))
                (cond
                  ((and (integerp x) (integerp y))
                   (when (/= x y)
                     (return (< x y))))
                  ((integerp x) (return t))
                  ((integerp y) (return nil))
                  ((string/= x y) (return (string< x y)))))))))
