(in-package #:cl-user)
(progn
  (defun coverage-form-operator-name (form)
    (when (and (consp form) (symbolp (first form)))
      (symbol-name (first form))))

  (defun coverage-definition-name (form)
    (let* ((operator (coverage-form-operator-name form))
           (name (second form)))
      (when (member operator
                    (list "DEFUN" "DEFMETHOD" "DEFMACRO" "DEFGENERIC"
                          "DEFCLASS" "DEFINE-CONDITION" "DEFTYPE" "DEFVAR"
                          "DEFPARAMETER" "DEFCONSTANT" "DEFSETF"
                          "DEFINE-COMPILER-MACRO" "DEFINE-SYMBOL-MACRO"
                          "DEFINE-MODIFY-MACRO" "DEFSTRUCT" "DEFINE-FLOW")
                    :test (function string=))
        (let ((symbol (if (member operator (list "DEFSTRUCT" "DEFINE-FLOW")
                                  :test (function string=))
                          (if (consp name) (first name) name)
                        name)))
          (when (symbolp symbol)
            (string-upcase (symbol-name symbol)))))))

  (defun coverage-form-opening-position (source start end)
    (loop with position = start
          while (< position end)
          do (let ((character (char source position)))
               (cond
                ((member character (list #\Space #\Tab #\Newline #\Return #\Page))
                 (incf position))
                ((char= character #\;)
                 (let ((newline (position #\Newline source :start position :end end)))
                   (setf position (if newline (1+ newline) end))))
                (t (return (and (char= character #\() position)))))
          finally (return nil)))
  (defun coverage-wrapper-children (source start end package-name operator)
    (let ((opening (coverage-form-opening-position source start end)))
      (when opening
        (with-input-from-string (stream (subseq source (1+ opening) (1- end)))
          (let ((*read-eval* nil)
                (*package* (or (find-package package-name) *package*)))
            (let ((operator-form (read stream nil :eof)))
              (when (and (symbolp operator-form)
                         (string= (symbol-name operator-form) operator))
                (when (string= operator "EVAL-WHEN")
                  (read stream nil :eof))
                (loop for child-start = (+ opening 1 (file-position stream))
                      for child = (read stream nil :eof)
                      until (eq child :eof)
                      for child-end = (+ opening 1 (file-position stream))
                      collect (list child-start child-end child))))))))
    )

  (defun coverage-definition-spans-in-form (form source start end package-name)
    (let ((operator (coverage-form-operator-name form))
          (name (coverage-definition-name form)))
      (cond
        ((and operator (string= operator "IN-PACKAGE"))
         (let ((package (second form)))
           (values nil (cond ((stringp package) (string-upcase package))
                             ((symbolp package) (string-upcase (symbol-name package)))
                             (t package-name)))))
        (name
         (values (list (list :start start :end end :package package-name :symbol name))
                 package-name))
        ((member operator (list "PROGN" "EVAL-WHEN") :test (function string=))
         (let ((definitions nil)
               (current-package package-name))
           (dolist (child (coverage-wrapper-children source start end package-name operator))
             (multiple-value-bind (nested next-package)
                 (coverage-definition-spans-in-form
                  (third child) source (first child) (second child) current-package)
               (setf definitions (nconc definitions nested)
                     current-package next-package)))
           (values definitions current-package)))
        (t (values nil package-name)))))

  (defun coverage-source-definitions-for-raw (pathname)
    (let ((source (uiop:read-file-string pathname))
          (definitions nil)
          (package-name "CL-USER"))
      (with-input-from-string (stream source)
        (let ((*read-eval* nil))
          (loop for start = (file-position stream)
                for form = (let ((*package* (or (find-package package-name) *package*)))
                             (read stream nil :eof))
                until (eq form :eof)
                for end = (file-position stream)
                do (multiple-value-bind (spans next-package)
                       (coverage-definition-spans-in-form
                        form source start end package-name)
                     (dolist (span spans)
                       (push (append (list :pathname (namestring (truename pathname))) span)
                             definitions))
                     (setf package-name next-package)))))
      (nreverse definitions)))
  (defun coverage-source-definitions-for (pathname)
    (handler-case
        (coverage-source-definitions-for-raw pathname)
      ((or reader-error end-of-file package-error) (condition)
        (error "Unable to parse coverage source file ~A: ~A"
               (namestring pathname) condition))))

  (defun coverage-owner-for-location (location definitions)
    (let ((pathname (getf location :pathname))
          (start (getf location :start))
          (end (getf location :end)))
      (when (and pathname (integerp start) (integerp end) (<= start end))
        (let* ((path (and (probe-file pathname) (namestring (truename pathname))))
               (matches
                 (remove-if-not
                  (lambda (definition)
                    (and path
                         (string= path (getf definition :pathname))
                         (<= (getf definition :start) start)
                         (<= end (getf definition :end))))
                  definitions)))
          (when (= (length matches) 1)
            (first matches))))))

  (defun coverage-uncovered-unit-count-for-state (state)
    (case state
      ((2 6 9) 1)
      (10 2)
      (otherwise 0)))

  (defun coverage-uncovered-form-locations-for-file (source)
    (let* ((get-records
             (uiop:symbol-call "CL-WEAVE" "COVERAGE-INTERNAL-SYMBOL"
                               "GET-RECORDS" t))
           (read-source
             (uiop:symbol-call "CL-WEAVE" "COVERAGE-INTERNAL-SYMBOL"
                               "READ-SOURCE" t))
           (read-maps
             (uiop:symbol-call "CL-WEAVE" "COVERAGE-INTERNAL-SYMBOL"
                               "READ-AND-RECORD-SOURCE-MAPS" t))
           (source-position
             (uiop:symbol-call "CL-WEAVE" "COVERAGE-INTERNAL-SYMBOL"
                               "SOURCE-PATH-SOURCE-POSITION" t))
           (make-sample-count
             (uiop:symbol-call "CL-WEAVE" "COVERAGE-INTERNAL-SYMBOL"
                               "MAKE-SAMPLE-COUNT" t))
           (source-text (funcall read-source source :default))
           (source-maps (funcall read-maps source-text))
           (counts (list :branch (funcall make-sample-count :branch)
                         :expression (funcall make-sample-count :expression))))
      (loop for record in (funcall get-records source counts)
            for path = (reverse (first record))
            for state = (second record)
            for uncovered-count = (coverage-uncovered-unit-count-for-state state)
            when (plusp uncovered-count)
              append
              (let* ((tlf (nth (car path) source-maps))
                     (form (car tlf))
                     (source-map (cdr tlf)))
                (when source-map
                  (multiple-value-bind (start end)
                      (funcall source-position (cons 0 (cdr path))
                               form source-map)
                    (when (and start end)
                      (list
                       (append
                        (list :pathname (pathname source)
                              :kind (if (= state 2) :expression :branch)
                              :uncovered-count uncovered-count)
                        (uiop:symbol-call "CL-WEAVE" "COVERAGE-FORM-POSITION"
                                          source-text start end))))))))))

  (defun coverage-adjust-statistics-for-exclusions (statistics exclusions owned-locations)
    (unless (coverage-exclusion-records-valid-p exclusions)
      (error "Invalid coverage exclusion records"))
    (let ((adjusted (copy-list statistics))
          (applied nil))
      (dolist (exclusion exclusions)
        (let* ((kind (getf exclusion :kind))
               (matches
                 (loop for pair in owned-locations
                       for location = (first pair)
                       for owner = (second pair)
                       when (and owner
                                 (eq kind (getf location :kind))
                                 (string-equal (getf exclusion :package)
                                               (getf owner :package))
                                 (string-equal (getf exclusion :symbol)
                                               (getf owner :symbol)))
                         collect location))
               (count (loop for location in matches
                            sum (getf location :uncovered-count 1)))
               (metric (if (eq kind :expression) :expression-total :branch-total)))
          (unless (plusp count)
            (error "Stale coverage exclusion ~A::~A (~A)"
                   (getf exclusion :package) (getf exclusion :symbol) kind))
          (unless (and (integerp (getf adjusted metric))
                       (>= (getf adjusted metric) count))
            (error "Coverage exclusion exceeds measured ~A total: ~S" metric count))
          (decf (getf adjusted metric) count)
          (push (append (copy-list exclusion) (list :excluded-count count)) applied)))
      (values adjusted (nreverse applied))))

  (defun coverage-statistics-with-exclusions (statistics exclusions locations)
    (let* ((pathnames
            (remove-duplicates
             (remove nil (mapcar (lambda (location) (getf location :pathname)) locations))
             :test (function equal)))
           (definitions
            (loop for pathname in pathnames
                  append (coverage-source-definitions-for pathname)))
           (owned-locations
            (loop for location in locations
                  collect (list location
                                (coverage-owner-for-location location definitions)))))
      (multiple-value-bind (adjusted applied)
          (coverage-adjust-statistics-for-exclusions
           statistics exclusions owned-locations)
        (values adjusted applied))))
)
(progn
  (defun aggregate-context-coverage-statistics (measured)
    (let ((statistics
            (list :expression-covered 0 :expression-total 0
                  :branch-covered 0 :branch-total 0)))
      (dolist (entry measured statistics)
        (dolist (metric (list :expression-covered :expression-total
                              :branch-covered :branch-total))
          (incf (getf statistics metric) (getf (third entry) metric))))))

  (defun context-coverage-exclusions (measured)
    (loop for entry in measured
          append (loop for exclusion in (fifth entry)
                       collect (append (list :context (first entry)) exclusion))))
)
(defun report-coverage-exclusion (entry)
  (if (getf entry :path)
      (format t "  ~A via ~A: ~A~%"
              (getf entry :path) (getf entry :rule) (getf entry :reason))
    (format t "  ~A ~A::~A (~A): ~D uncovered form~:P excluded~%"
            (getf entry :context) (getf entry :package) (getf entry :symbol)
            (getf entry :kind) (getf entry :excluded-count))))

