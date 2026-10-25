(in-package #:aitools.integration.structure-test)

(declaim (ftype function %handwritten-production-package-p %read-suppressed-form-at))

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
                  (multiple-value-bind (form end)
                      (read-from-string view t nil :start offset)
                    (let ((seen (make-hash-table :test #'eq)))
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
                      (values
                       form
                       (loop with shift = 0
                             for replacement in replacements
                             for start = (first replacement)
                             for old-end = (second replacement)
                             for token = (third replacement)
                             for view-end = (+ start shift 2 (length token))
                             when (> view-end end) return (- end shift)
                             do (incf shift
                                      (- (+ 2 (length token)) (- old-end start)))
                             finally (return (- end shift))))))))))
      (error (caught-error) (locally (declare (ignore caught-error)) (values :unreadable nil))))))

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

(defun %handwritten-whitespace-p (character) (find character '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun %strip-reader-comments (text)
  (let* ((sentinel (code-char #xe000))
         (protected (copy-seq text))
         (unreadable nil)
         (cursor 0))
    (loop for marker = (search "#;" text :start2 cursor)
          while marker
          do (setf (char protected (1+ marker)) sentinel
                   cursor (+ marker 2)))
    (let ((stripped (%strip-prose protected))
          (token (coerce (list #\# sentinel) 'string)))
      (setf cursor 0)
      (loop for marker = (search token stripped :start2 cursor)
            while marker
            do (multiple-value-bind (form end)
                   (%read-suppressed-form-at text (+ marker 2))
                 (if (or (eq form :unreadable)
                         (not (integerp end))
                         (<= end marker))
                     (progn
                       (push (list (%line-at text marker)
                                   "unreadable #; comment form")
                             unreadable)
                       (setf cursor (+ marker 2)))
                     (progn
                       (loop for index from marker below end
                             unless (char= (char stripped index) #\Newline)
                               do (setf (char stripped index) #\Space))
                       (setf cursor end)))))
      (values stripped (nreverse unreadable)))))

(defun %handwritten-form-head-p (form name)
  (and (consp form)
       (symbolp (first form))
       (let* ((symbol (first form))
              (origin (get symbol :u05-qualified-reference)))
         (string-equal (if origin (second origin) (symbol-name symbol)) name))))

(defun %handwritten-function-name-parts (name)
  (cond
    ((symbolp name) (values name nil t))
    ((and (%handwritten-form-head-p name "SETF")
          (= (length name) 2)
          (symbolp (second name)))
     (values (second name) t t))
    (t (values nil nil nil))))

(defun %handwritten-symbol-package (symbol current-package)
  (let ((origin (get symbol :u05-qualified-reference))
        (package (symbol-package symbol)))
    (if origin
        (let ((resolved (find-package (first origin))))
          (if resolved (package-name resolved) (string-upcase (first origin))))
        (if (or (null package) (eq package *reader-package*))
            (let ((resolved (find-package current-package)))
              (if resolved (package-name resolved) current-package))
            (package-name package)))))

(defun %handwritten-candidate (symbol setf-name current-package relative line)
  (let* ((origin (get symbol :u05-qualified-reference))
         (name (if origin (second origin) (symbol-name symbol)))
         (package (%handwritten-symbol-package symbol current-package)))
    (when (%handwritten-production-package-p package)
      (list package (if setf-name (format nil "(SETF ~A)" name) name)
            relative line))))

(defun %handwritten-quoted-name-parts (designator)
  (when (and (%handwritten-form-head-p designator "QUOTE")
             (= (length designator) 2))
    (%handwritten-function-name-parts (second designator))))
