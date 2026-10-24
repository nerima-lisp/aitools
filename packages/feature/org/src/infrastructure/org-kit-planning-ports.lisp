(in-package #:aitools.org.infrastructure)

(defun %org-selection-failure (kind message)
  (%failure kind message))

(defun %org-selected-heading (engine options)
  (let ((match (getf options :match)))
    (unless (and (stringp match) (plusp (length match)))
      (return-from %org-selected-heading
        (values nil nil nil (%failure :argument "--match is required"))))
    (multiple-value-bind (at context reference failure) (%explicit-time-values (getf options :at) (getf options :tz) (getf options :dst-policy))
      (when failure
        (return-from %org-selected-heading
          (values nil nil nil failure)))
      (handler-case (let ((matches (org-kit:query-match-at engine match reference)))
                      (cond
                        ((null matches) (values nil nil nil (%org-selection-failure :selection-no-match "no heading matches --match")))
                        ((cdr matches) (values nil nil nil (%org-selection-failure :selection-ambiguous "more than one heading matches --match")))
                        (t (values (first matches) at context nil))))
        (org-kit:org-resource-limit-error ()
          (values nil nil nil (%failure :too-large "Org selection exceeded a resource limit")))
        (org-kit:org-query-error (condition)
          (values nil nil nil (%query-syntax-failure match condition)))
        (org-kit:org-kit-error ()
          (values nil nil nil (%failure :internal "Org selection failed")))))))

(defun %org-edit-condition-failure (condition)
  (case (org-kit:org-edit-error-reason condition)
    (:ambiguous-target (%failure :selection-ambiguous "the selected Org value is ambiguous"))
    (:target-not-found (%failure :not-found "the selected Org value does not exist"))
    (:invalid-value (%failure :argument "an Org edit argument is invalid"))
    (:blocked (%failure :syntax "the selected TODO entry is blocked"))
    (otherwise (%failure :internal "Org edit failed"))))

(defun %org-kind-keyword (kind)
  (cond
    ((and (stringp kind) (string= kind "scheduled")) :scheduled)
    ((and (stringp kind) (string= kind "deadline")) :deadline)
    ((and (stringp kind) (string= kind "closed")) :closed)))

(defun %org-kind-name (kind)
  (case kind
    (:scheduled "scheduled")
    (:deadline "deadline")
    (:closed "closed")))

(defun %planning-markers (kind)
  (case kind
    (:scheduled "SCHEDULED:")
    (:deadline "DEADLINE:")
    (:closed "CLOSED:")))

(defun %planning-marker-count (engine heading marker)
  (let* ((section
          (find :section
                (org-kit:node-children heading)
                :key #'org-kit:node-type))
         (source (org-kit:document-text engine))
         (start (and section (org-kit:node-begin section)))
         (end (and section (org-kit:node-end section))))
    (unless section
      (return-from %planning-marker-count 0))
    (let* ((line-end (or (position #\Newline source :start start :end end) end))
           (line-start
             (loop for position from start below line-end
                   while (member (char source position) '(#\Space #\Tab))
                   finally (return position))))
      (unless (and (<= (+ line-start (length marker)) line-end)
                   (string-equal marker
                                 source
                                 :start2 line-start
                                 :end2 (+ line-start (length marker))))
        (return-from %planning-marker-count 0))
      (loop with position = line-start
            for found = (search marker source
                                :start2 position
                                :end2 line-end
                                :test #'char-equal)
            while found
            count t
            do (setf position (+ found (length marker)))))))

(defun %planning-duplicate-failure (engine heading kind)
  (when (> (%planning-marker-count engine heading (%planning-markers kind)) 1)
    (%failure :selection-ambiguous "the selected heading has multiple timestamps of this kind")))

(defun %org-todo-edit (engine options)
  (multiple-value-bind (heading at context failure) (%org-selected-heading engine options)
    (when failure
      (return-from %org-todo-edit
        (values nil nil failure)))
    (handler-case (multiple-value-bind (result transition) (org-kit:transition-heading-todo engine heading (getf options :state) :at at :time-context context :note (getf options :note))
                    (values (org-kit:edit-result-updated-document result)
                            (list (cons "previous_state" (org-kit:transition-result-previous-state transition)) (cons "new_state" (org-kit:transition-result-new-state transition)))
                            nil))
      (org-kit:org-edit-error (condition)
        (values nil nil (%org-edit-condition-failure condition)))
      (org-kit:org-time-error ()
        (values nil nil (%failure :argument "TODO time context is invalid")))
      (org-kit:org-resource-limit-error ()
        (values nil nil (%failure :too-large "TODO edit exceeded a resource limit")))
      (org-kit:org-kit-error ()
        (values nil nil (%failure :internal "TODO edit failed"))))))

(defun %org-property-edit (engine options delete-p)
  (multiple-value-bind (heading at context failure) (%org-selected-heading engine options)
    (declare (ignore at context))
    (when failure
      (return-from %org-property-edit
        (values nil nil failure)))
    (let ((name (getf options :name)))
      (handler-case (let ((updated
                           (if delete-p
                               (org-kit:remove-property engine heading name)
                               (org-kit:set-property engine heading name (getf options :value)))))
                      (values updated (list (cons "property" name)) nil))
        (org-kit:org-edit-error (condition)
          (if (and delete-p (eq (org-kit:org-edit-error-reason condition) :target-not-found))
              (values engine (list (cons "property" name)) nil)
              (values nil nil (%org-edit-condition-failure condition))))
        (org-kit:org-resource-limit-error ()
          (values nil nil (%failure :too-large "property edit exceeded a resource limit")))
        (org-kit:org-kit-error ()
          (values nil nil (%failure :internal "property edit failed")))))))

(defun %org-planning-edit (engine options delete-p)
  (multiple-value-bind (heading at context failure) (%org-selected-heading engine options)
    (declare (ignore at context))
    (when failure
      (return-from %org-planning-edit
        (values nil nil failure)))
    (let* ((kind (%org-kind-keyword (getf options :kind)))
           (timestamp (getf options :timestamp)))
      (unless kind
        (return-from %org-planning-edit
          (values nil nil (%failure :argument "planning --kind is invalid"))))
      (progn
        (unless delete-p
          (multiple-value-bind (view timestamp-failure)
              (validate-planning-timestamp (getf options :kind) timestamp)
            (declare (ignore view))
            (when timestamp-failure
              (return-from %org-planning-edit
                (values nil nil timestamp-failure)))))
        (let ((duplicate (%planning-duplicate-failure engine heading kind)))
          (when duplicate
            (return-from %org-planning-edit
              (values nil nil duplicate)))))
      (handler-case (let ((updated
                           (if delete-p
                               (org-kit:remove-planning engine heading kind)
                               (org-kit:set-planning engine heading kind timestamp))))
                      (values updated
                              (append (list (cons "kind" (%org-kind-name kind)))
                                      (unless delete-p
                                        (list (cons "timestamp" timestamp))))
                              nil))
        (org-kit:org-edit-error (condition)
          (if (and delete-p (eq (org-kit:org-edit-error-reason condition) :target-not-found))
              (values engine (list (cons "kind" (%org-kind-name kind))) nil)
              (values nil nil (%org-edit-condition-failure condition))))
        (org-kit:org-time-error ()
          (values nil nil (%failure :argument "--timestamp is not valid for this command")))
        (org-kit:org-resource-limit-error ()
          (values nil nil (%failure :too-large "planning edit exceeded a resource limit")))
        (org-kit:org-kit-error ()
          (values nil nil (%failure :internal "planning edit failed")))))))
