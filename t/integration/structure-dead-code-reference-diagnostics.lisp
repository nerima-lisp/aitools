(in-package #:aitools.integration.structure-test)

(defvar *liveness-index-diagnostics* nil)

(defun %liveness-index-static-builder-p (builder allow-lambda-p)
  (and (consp builder)
       (symbolp (first builder))
       (member (%liveness-symbol-name (first builder))
               (if allow-lambda-p '("FUNCTION" "QUOTE" "LAMBDA")
                   '("FUNCTION" "QUOTE"))
               :test #'string=)))

(defun %liveness-index-form-position (record operator cursor fallback)
  (or (loop for candidate in
              (%liveness-form-positions (getf record :code) operator)
            when (>= candidate cursor)
              return candidate)
      fallback))

(defun %liveness-index-scan-builder (record position object context depth)
  (if (not (consp object))
      position
      (let* ((head (first object))
             (operator (and (symbolp head) (%liveness-symbol-name head)))
             (next-position position))
        (cond
          ((eq head :u05-quasiquote)
           (case context
             (:active
              (%liveness-index-scan-builder record position (second object)
                                            :template 1))
             (:template
              (%liveness-index-scan-builder record position (second object)
                                            :template (1+ depth)))
             (otherwise
              (%liveness-index-scan-builder record position (second object)
                                            :inactive 0))))
          ((member head '(:u05-unquote :u05-unquote-splicing))
           (case context
             (:template
              (if (= depth 1)
                  (%liveness-index-scan-builder record position (second object)
                                                :active 0)
                  (%liveness-index-scan-builder record position (second object)
                                                :template (1- depth))))
             (otherwise
              (%liveness-index-scan-builder record position (second object)
                                            :inactive 0))))
          (t
           (when (and operator
                      (member operator '("DEFINE-COMMAND"
                                         "DEFINE-INSPECT-COMMAND")
                              :test #'string=))
             (let ((at (%liveness-index-form-position record operator position
                                                      position)))
               (setf next-position (1+ at))
               (when (eq context :active)
                 (cond
                   ((string= operator "DEFINE-COMMAND")
                    (let* ((options (third object))
                           (builder
                             (loop for tail = options then (cdr tail)
                                   while (consp tail)
                                   when (eq (first tail) :command-builder)
                                     return (second tail))))
                      (unless (%liveness-index-static-builder-p builder t)
                        (%liveness-index-lookup-diagnostic
                         record at "unresolved command builder"))))
                   (t
                    (unless (%liveness-index-static-builder-p
                             (fourth object) nil)
                      (%liveness-index-lookup-diagnostic
                       record at "unresolved inspect command builder")))))))
           (if (and operator (string= operator "QUOTE"))
               (setf next-position
                     (%liveness-index-scan-builder
                      record next-position (second object)
                      (if (eq context :active) :inactive context) depth))
               (loop for tail = object then (cdr tail)
                     while (consp tail)
                     do (setf next-position
                              (%liveness-index-scan-builder
                               record next-position (car tail) context depth))))
           next-position)))))

(defun %liveness-index-lookup-diagnostic (record position message-or-form)
  (when (eq (getf record :kind) :production)
    (if (stringp message-or-form)
        (push (format nil "~A:~D: ~A"
                      (getf record :relative)
                      (%line-at (getf record :text) position)
                      message-or-form)
              *liveness-index-diagnostics*)
        (%liveness-index-scan-builder record position message-or-form
                                      :active 0))))

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

(defun %handwritten-setf-results (form relative line current-package)
  (let ((candidates nil)
        (unresolved nil))
    (loop for tail on (rest form) by #'cddr
          for place = (first tail)
          for accessor = (find-if
                          (lambda (name) (%handwritten-form-head-p place name))
                          '("FDEFINITION" "SYMBOL-FUNCTION" "MACRO-FUNCTION"))
          when (and accessor (= (length place) 2))
            do (multiple-value-bind (symbol setf-name static-p)
                   (%handwritten-quoted-name-parts (second place))
                 (cond
                   ((and static-p (not setf-name))
                    (let ((candidate
                            (%handwritten-candidate symbol nil current-package relative line)))
                      (when candidate (push candidate candidates))))
                   ((string= accessor "FDEFINITION")
                    (push (list :dynamic-fdefinition relative line) candidates))
                   ((not static-p)
                    (push (format nil "~A:~D: dynamic ~A target ~S"
                                  relative line accessor (second place))
                          unresolved)))))
    (values (nreverse candidates) (nreverse unresolved))))

(defun %handwritten-package-designator (designator)
  (let ((name (typecase designator (symbol (symbol-name designator))
                                  (string designator))))
    (when name
      (let ((package (find-package name)))
        (if package (package-name package) (string-upcase name))))))

(defun %scan-handwritten-text (text relative)
  (multiple-value-bind (code reader-comments)
      (%strip-reader-comments text)
    (let ((forms nil)
          (candidates nil)
          (unresolved
            (mapcar (lambda (entry)
                      (format nil "~A:~D: ~A" relative (first entry)
                              (second entry)))
                    reader-comments))
          (current-package "CL-USER"))
      (dolist (operator '("in-package" "setf" "defun"))
        (dolist (position (%handwritten-form-positions code operator))
          (push (list operator position) forms)))
      (dolist (entry (sort forms #'< :key #'second))
        (let* ((operator (first entry))
               (position (second entry))
               (form (%liveness-read-form-at text position))
               (line (%line-at text position)))
          (cond
            ((eq form :unreadable)
             (push (format nil "~A:~D: unreadable ~A form" relative line operator)
                   unresolved))
            ((string= operator "in-package")
             (let ((name (%handwritten-package-designator (second form))))
               (when name (setf current-package name))))
            ((string= operator "defun")
             (let ((symbol (second form)))
               (when (symbolp symbol)
                 (let ((candidate
                         (%handwritten-candidate symbol nil current-package relative line)))
                   (when candidate (push candidate candidates))))))
            (t
             (multiple-value-bind (found unresolved-forms)
                 (%handwritten-setf-results form relative line current-package)
               (setf candidates (nconc (nreverse found) candidates)
                     unresolved (nconc (nreverse unresolved-forms) unresolved)))))))
      (values (nreverse candidates) (nreverse unresolved)))))
