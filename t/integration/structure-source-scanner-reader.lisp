(in-package #:aitools.integration.structure-test)

(defvar *reader-package*
  (or (find-package "AITOOLS.STRUCTURE-TEST.READER")
      (make-package "AITOOLS.STRUCTURE-TEST.READER" :use '())))

(defun %read-form-at (text position)
  "The form in TEXT at POSITION, read without evaluation into a scratch
package, or :UNREADABLE."
  (handler-case
      (with-standard-io-syntax
        (let ((*read-eval* nil) (*package* *reader-package*))
          (read-from-string text t nil :start position)))
    (error () :unreadable)))

(defvar *liveness-reader-view-cache* (make-hash-table :test #'eq))

(defun %liveness-read-form-at (text position)
  "Read one form with occurrence metadata for qualified symbols and FIND-SYMBOL calls."
  (let* ((cache
           (or (gethash text *liveness-reader-view-cache*)
               (setf (gethash text *liveness-reader-view-cache*)
                     (%liveness-reader-view-cache-entry text))))
         (view (first cache))
         (replacements (second cache))
         (offset
           (+ position
              (loop for replacement in replacements
                    for start = (first replacement)
                    for end = (second replacement)
                    for token = (third replacement)
                    when (<= end position)
                      sum (- (+ 2 (length token)) (- end start)))))
         (origins (make-hash-table :test #'equal))
         (source-positions (make-hash-table :test #'equal))
         (source-names (make-hash-table :test #'equal)))
    (dolist (replacement replacements)
      (setf (gethash (third replacement) origins) (fourth replacement)
            (gethash (third replacement) source-positions) (first replacement)
            (gethash (third replacement) source-names) (fifth replacement)))
    (handler-case
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (*package* *reader-package*)
                (quasiquote-depth 0)
                (readtable (copy-readtable nil)))
            (labels ((read-quasiquote (stream character)
                       (declare (ignore character))
                       (incf quasiquote-depth)
                       (unwind-protect
                            (list :u05-quasiquote (read stream t nil t))
                         (decf quasiquote-depth)))
                     (read-unquote (stream character)
                       (declare (ignore character))
                       (when (zerop quasiquote-depth)
                         (error "Comma outside a quasiquote."))
                       (let ((splicing-p
                               (member (peek-char nil stream nil nil)
                                       '(#\@ #\.))))
                         (when splicing-p (read-char stream))
                         (decf quasiquote-depth)
                         (unwind-protect
                              (list (if splicing-p
                                        :u05-unquote-splicing
                                        :u05-unquote)
                                    (read stream t nil t))
                           (incf quasiquote-depth)))))
              (let ((*readtable* readtable))
                (set-macro-character #\` #'read-quasiquote nil)
                (set-macro-character #\, #'read-unquote nil)
                (let ((form (read-from-string view t nil :start offset))
                      (seen (make-hash-table :test #'eq)))
                  (labels ((annotate (object)
                             (cond
                               ((symbolp object)
                                (let* ((name (symbol-name object))
                                       (origin (gethash name origins))
                                       (source-name (gethash name source-names)))
                                  (when (or origin source-name)
                                    (setf (get object :u05-source-position)
                                          (gethash name source-positions))
                                    (when origin
                                      (setf (get object :u05-qualified-reference) origin))
                                    (when source-name
                                      (setf (get object :u05-source-name) source-name)))))
                               ((consp object)
                                (unless (gethash object seen)
                                  (setf (gethash object seen) t)
                                  (annotate (car object))
                                  (annotate (cdr object)))))))
                    (annotate form))
                  form)))))
      (error () :unreadable)))
)

(defun %liveness-reader-view (text)
  "Return reader text and unique metadata for qualified symbols and call occurrences."
  (let* ((code (%strip-prose text))
         (references (%qualified-references code))
         (operators (%form-positions code "find-symbol"))
         (prefix (loop for index from 0
                       for candidate = (format nil "U05Q-~D-" index)
                       unless (search candidate text :test #'char-equal)
                         return candidate))
         (descriptors
           (append
            (loop for reference in references
                  collect (list (fifth reference) (seventh reference)
                                (list (first reference) (second reference)) nil))
            (loop for position in operators
                  for start = (1+ position)
                  collect (list start (+ start (length "FIND-SYMBOL"))
                                nil "FIND-SYMBOL"))))
         (replacements
           (loop for descriptor in (sort descriptors #'< :key #'first)
                 for serial from 1
                 collect (list (first descriptor) (second descriptor)
                               (format nil "~A~D" prefix serial)
                               (third descriptor) (fourth descriptor)))))
    (values
     (with-output-to-string (output)
       (loop with cursor = 0
             for replacement in replacements
             for start = (first replacement)
             for end = (second replacement)
             for token = (third replacement)
             do (write-string (subseq text cursor start) output)
                (write-string "#:" output)
                (write-string token output)
                (setf cursor end)
             finally (write-string (subseq text cursor) output)))
     replacements)))

(defun %liveness-reader-view-cache-entry (text)
  (multiple-value-bind (view replacements)
      (%liveness-reader-view text)
    (list view replacements)))

(defun %form-positions (stripped operator)
  "Start positions of every operator form or exact keyword token in STRIPPED."
  (let* ((keyword-p (and (plusp (length operator))
                         (char= (char operator 0) #\:)))
         (needle (if keyword-p operator (format nil "(~A" operator)))
         (positions nil))
    (loop for start = (search needle stripped :test (function char-equal))
          then (search needle stripped :start2 (+ start (length needle))
                       :test (function char-equal))
          while start
          do (let ((after (+ start (length needle))))
               (when (and (or (not keyword-p)
                              (zerop start)
                              (not (%symbol-constituent-p
                                    (char stripped (1- start)))))
                          (or (= after (length stripped))
                              (not (%symbol-constituent-p (char stripped after)))))
                 (push start positions))))
    (nreverse positions)))

(defun %liveness-form-positions (stripped operator)
  (let ((positions (%form-positions stripped operator)))
    (labels ((escaped-head (start)
               (let ((position (1+ start))
                     (length (length stripped))
                     (bars nil)
                     (escaped nil)
                     (qualified nil)
                     (had-escape nil))
                 (loop while (and (< position length)
                                  (find (char stripped position)
                                        '(#\Space #\Tab #\Newline #\Return #\Page)))
                       do (incf position))
                 (values
                  (with-output-to-string (output)
                    (loop while (< position length)
                          for char = (char stripped position)
                          do (cond
                               (escaped
                                (write-char char output)
                                (setf escaped nil)
                                (incf position))
                               ((char= char #\\)
                                (setf escaped t
                                      had-escape t)
                                (incf position))
                               ((char= char #\|)
                                (setf bars (not bars)
                                      had-escape t)
                                (incf position))
                               ((and (not bars) (char= char #\:))
                                (setf qualified t)
                                (return))
                               ((and (not bars)
                                     (member char
                                             '(#\Space #\Tab #\Newline #\Return #\Page
                                               #\( #\) #\' #\" #\` #\, #\; #\#)))
                                (return))
                               (t
                                (write-char (if bars char (char-upcase char)) output)
                                (incf position)))))
                  qualified
                  had-escape))))
      (loop for start = (position #\( stripped)
            then (position #\( stripped :start (1+ start))
            while start
            do (multiple-value-bind (head qualified escaped)
                   (escaped-head start)
                 (when (and escaped
                            (not qualified)
                            (string= head operator))
                   (push start positions))))
      (dolist (reference (%qualified-references stripped))
        (when (string= (second reference) operator)
          (let ((start (position #\( stripped :end (fifth reference) :from-end t)))
            (when start (push start positions)))))
      (sort (remove-duplicates positions :test (function =)) (function <)))))
