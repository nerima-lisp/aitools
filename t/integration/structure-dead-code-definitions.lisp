(in-package #:aitools.integration.structure-test)

(defun %liveness-symbol-tokens (text)
  "Unqualified symbol tokens in TEXT as (NAME POSITION), excluding package syntax."
  (let ((tokens '()) (length (length text)) (position 0))
    (loop while (< position length)
          do (if (%symbol-constituent-p (char text position))
                 (let ((start position))
                   (loop while (and (< position length)
                                    (%symbol-constituent-p (char text position)))
                         do (incf position))
                   (unless (or (and (plusp start) (char= (char text (1- start)) #\:))
                               (and (< position length)
                                    (char= (char text position) #\:)))
                     (push (list (string-upcase (subseq text start position)) start)
                           tokens)))
                 (incf position)))
    (nreverse tokens)))

(defun %liveness-slot-function-names (slots)
  "Explicit readers, writers, and accessors from class or condition slots."
  (let ((names '()))
    (dolist (slot slots)
      (when (consp slot)
        (loop for options on (rest slot) by #'cddr
              while (cdr options)
              when (member (first options) '(:reader :writer :accessor))
                do (push (%designator-name (second options)) names))))
    (nreverse names)))

(defun %liveness-struct-defined-names (form)
  "Generated type and function names from a DEFSTRUCT form."
  (labels ((optional-name (object)
             (let ((name (%designator-name object)))
               (unless (equal name "NIL") name))))
    (let* ((spec (second form))
           (name (%designator-name (if (consp spec) (first spec) spec)))
           (options (if (consp spec) (rest spec) nil))
           (predicate (assoc :predicate options))
           (constructor-options
             (remove-if-not (lambda (option) (eq (first option) :constructor)) options))
           (copier (assoc :copier options))
           (conc-name (assoc :conc-name options))
           (prefix (if conc-name
                       (optional-name (second conc-name))
                       (and name (concatenate (quote string) name "-"))))
           (slots (if (stringp (third form))
                      (cdddr form)
                      (cddr form))))
      (when name
        (append
         (list (list :type name))
         (let ((predicate-name (if predicate
                                   (optional-name (second predicate))
                                   (concatenate (quote string) name "-P"))))
           (when predicate-name
             (list (list :function predicate-name))))
         (if constructor-options
             (loop for option in constructor-options
                   for constructor-name = (optional-name (second option))
                   when constructor-name
                     collect (list :function constructor-name))
             (list (list :function (concatenate (quote string) "MAKE-" name))))
         (let ((copier-name (if copier
                                (optional-name (second copier))
                                (concatenate (quote string) "COPY-" name))))
           (when copier-name
             (list (list :function copier-name))))
         (when prefix
           (loop for slot in slots
                 for slot-name = (%designator-name (if (consp slot) (first slot) slot))
                 when slot-name
                   collect (list :function (concatenate (quote string) prefix slot-name)))))))))

(defun %liveness-form-definitions (form)
  "Names defined by a supported top-level form as (NAMESPACE NAME ROOT-MARKER)."
  (let* ((operator (and (consp form) (%liveness-symbol-name (first form))))
         (name (and (consp form) (second form)))
         (operator operator)
         (simple-name (%designator-name name)))
    (cond
      ((member operator '("DEFUN" "DEFMACRO" "DEFGENERIC") :test #'string=)
       (list (list :function simple-name nil)))
      ((member operator '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT") :test #'string=)
       (list (list :value simple-name nil)))
      ((equal operator "DEFINE-FLOW")
       (list (list :function (%designator-name (first name)) nil)))
      ((equal operator "DEFINE-WRITE-COMMAND")
       (list (list :function
                   (format nil "PREPARE-~A/K"
                           (substitute #\- #\. (string-upcase name)))
                   (list :write-command name))))
      ((equal operator "DEFINE-FORMAT-WRITE-COMMANDS")
       (when (stringp name)
         (loop for verb in '("set" "delete" "merge" "patch" "fmt")
               for command = (format nil "~A.~A" (string-downcase name) verb)
               collect (list :function
                             (format nil "PREPARE-~A/K"
                                     (substitute #\- #\. (string-upcase command)))
                             (list :format-write-command command)))))
      ((equal operator "DEFINE-DATA-TABLE")
       (list (list :value simple-name nil)))
      ((equal operator "DEFSTRUCT")
       (let* ((names (%liveness-struct-defined-names form))
              (struct-name (second (first names))))
         (loop for (namespace symbol) in names
               when symbol
                 collect (list namespace symbol (list :generated-struct struct-name)))))
      ((member operator '("DEFCLASS" "DEFINE-CONDITION") :test #'string=)
       (append (list (list :type simple-name nil))
               (loop for symbol in (%liveness-slot-function-names (fourth form))
                     collect (list :function symbol nil))))
      (t nil))))

(defun %liveness-data-function-designators (record form code)
  "Resolve each function designator from the same :CALC-FUNCTIONS entry."
  (declare (ignore code))
  (when (and (consp form)
             (string= (%liveness-symbol-name (first form)) "DEFINE-DATA-TABLE")
             (eq (third form) :calc-functions))
    (loop for entry in (fourth form)
          for designator = (third entry)
          for origin = (and (symbolp designator)
                            (get designator :u05-qualified-reference))
          for home = (and (symbolp designator) (symbol-package designator))
          when (and (symbolp designator)
                    (not (keywordp designator))
                    (or origin home))
            collect
              (or origin
                  (if (eq home *reader-package*)
                      (list (getf record :package)
                            (%liveness-symbol-name designator))
                      (list (package-name home)
                            (%liveness-symbol-name designator)))))))

(defun %liveness-definitions (records)
  "Definitions from production files and the roots registered for dynamic dispatch."
  (let ((definitions nil))
    (labels ((command-data-row-p (form command)
                                 (or (and (consp form)
                                          (eq (first form) :name)
                                          (stringp (second form))
                                          (string-equal (second form) command))
                                     (and (consp form)
                                          (some (lambda (part)
                                                  (command-data-row-p part command))
                                                form))))
             (preparer-dispatch-p (form)
                                  (or (and (consp form)
                                           (equal (%liveness-symbol-name (first form)) "GETHASH")
                                           (symbolp (second form))
                                           (equal (%liveness-symbol-name (second form)) "NAME")
                                           (symbolp (third form))
                                           (equal (%liveness-symbol-name (third form)) "*PREPARERS*"))
                                      (and (consp form)
                                           (some (lambda (part) (preparer-dispatch-p part)) form))))
             (command-data-present-p (command)
                                     (loop for record in records
                                           thereis
                                           (and (string= (getf record :relative)
                                                         "data/application/edit/command-spec-data.lisp")
                                                (loop for position in
                                                      (%liveness-top-level-forms (getf record :code))
                                                      for form = (%liveness-read-form-at (getf record :text) position)
                                                      thereis (command-data-row-p form command)))))
             (preparer-dispatch-present-p ()
                                          (loop for record in records
                                                thereis
                                                (and (string= (getf record :relative)
                                                              "packages/feature/edit/src/application/commands.lisp")
                                                     (loop for position in
                                                           (%liveness-top-level-forms (getf record :code))
                                                           for form =
                                                           (%liveness-read-form-at (getf record :text) position)
                                                           thereis (preparer-dispatch-p form)))))
             (data-function-root-p (package name)
                                   (loop for record in records
                                         thereis
                                         (and (eq (getf record :kind) :data)
                                              (let* ((code (getf record :code))
                                                     (text (getf record :text))
                                                     (starts (%liveness-top-level-forms code))
                                                     (ends (append (rest starts) (list (length code)))))
                                                (loop for start in starts
                                                      for end in ends
                                                      for form = (%liveness-read-form-at text start)
                                                      for form-code = (subseq code start end)
                                                      thereis
                                                      (member (list package name)
                                                              (%liveness-data-function-designators
                                                               record form form-code)
                                                              :test #'equal))))))
             (definition-root-p (package namespace name marker)
               (or (and (eq namespace :function) (data-function-root-p package name))
                   (%liveness-documented-api-root-p package namespace name)
                   (and (consp marker)
                        (eq (first marker) :write-command)
                        (string= package "AITOOLS.EDIT.APPLICATION")
                        (command-data-present-p (second marker))
                        (preparer-dispatch-present-p))
                   (and (consp marker)
                        (eq (first marker) :format-write-command)
                        (string= package "AITOOLS.EDIT.APPLICATION")
                        (command-data-present-p (second marker))
                        (preparer-dispatch-present-p)))))
      (dolist (record records)
        (when (eq (getf record :kind) :production)
          (let* ((text (getf record :text))
                 (package (getf record :package)))
            (dolist (start (%liveness-top-level-forms (getf record :code)))
              (let ((form (%liveness-read-form-at text start)))
                (dolist (entry (%liveness-form-definitions form))
                  (destructuring-bind (namespace name marker) entry
                    (when name
                      (push (list :name name :namespace namespace :package package
                            :file (getf record :relative)
                            :line (%line-at text start)
                            :root (not (null (definition-root-p package namespace name marker)))
                            :group (and (consp marker)
                                        (eq (first marker) :generated-struct)
                                        (list package (getf record :relative)
                                              start (second marker))))
                            definitions)))))))))
      (nreverse definitions))))
