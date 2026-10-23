(in-package #:aitools/cli)
(defvar *trace-session* nil)
(defvar *trace-envelope-owner-p* nil)
(defstruct (%trace-session
            (:constructor %make-trace-session-record
                          (&key provider tracer root-span root-span-id records kinds outcomes
                                sequences registry counters meta)))
           provider tracer root-span root-span-id records kinds outcomes sequences
           registry counters meta)
(defstruct (%projected-span
            (:constructor %make-projected-span
                          (&key kit-id parent-kit-id kind name start-ns duration-ns outcome sequence)))
           kit-id parent-kit-id kind name start-ns duration-ns outcome sequence)
(defun %make-trace-session (&key clock id-generator monotonic-units-per-second)
  (let* ((registry (observability-kit:make-metric-registry :scope-name "aitools.cli"))
         (span-count (observability-kit:define-counter
                      registry span_count :help "Number of spans in the trace." :unit "1"))
         (command-count (observability-kit:define-counter
                         registry command_count :help "Number of command spans in the trace." :unit "1"))
         (child-process-count (observability-kit:define-counter
                               registry child_process_count :help "Number of child process spans in the trace."
                               :unit "1"))
         (io-count (observability-kit:define-counter
                    registry io_count :help "Number of IO spans in the trace." :unit "1"))
         (duration-ns (observability-kit:define-counter
                       registry duration_ns :help "Root command duration in nanoseconds." :unit "ns"))
         (session (%make-trace-session-record
                   :registry registry
                   :counters (list (cons "span_count" span-count)
                                   (cons "command_count" command-count)
                                   (cons "child_process_count" child-process-count)
                                   (cons "io_count" io-count)
                                   (cons "duration_ns" duration-ns))
                   :kinds (make-hash-table :test #'equal)
                   :outcomes (make-hash-table :test #'equal)
                   :sequences (make-hash-table :test #'equal)))
         (processor
          (observability-kit:make-simple-span-processor
           (lambda (records)
             (setf (%trace-session-records session)
                   (nconc (%trace-session-records session) records)))))
         (provider-options
          (append (list :sampler :always-on :span-processors (list processor))
                  (when clock (list :clock clock))
                  (when id-generator (list :id-generator id-generator))
                  (when monotonic-units-per-second
                    (list :monotonic-units-per-second monotonic-units-per-second))))
         (provider (apply #'observability-kit:make-tracer-provider provider-options)))
    (setf (%trace-session-provider session) provider
          (%trace-session-tracer session) (observability-kit:make-tracer provider "aitools.cli"))
    session))
(defun %trace-kind-name (kind)
  (ecase kind (:command "command") (:child_process "child_process") (:io "io")))
(defun %trace-outcome-p (outcome)
  (member outcome '(:ok :partial :error :timeout :unavailable) :test #'eq))
(defun %trace-check-outcome (outcome)
  (unless (%trace-outcome-p outcome) (error "Invalid trace outcome ~S." outcome))
  outcome)
(defun %trace-outcome-name (outcome)
  (string-downcase (symbol-name (%trace-check-outcome outcome))))
(defun %call-with-trace-span (kind name thunk &key outcome-function condition-outcome-function)
  (let ((session *trace-session*))
    (if (null session)
        (funcall thunk)
      (let* ((root-p (null (%trace-session-root-span session)))
             (span (observability-kit:start-span
                    (%trace-session-tracer session) name :kind :internal
                    :parent (if root-p nil :current)))
             (span-id (observability-kit:span-id span))
             (sequence (hash-table-count (%trace-session-sequences session)))
             (outcome :error))
        (setf (gethash span-id (%trace-session-kinds session)) (%trace-kind-name kind)
              (gethash span-id (%trace-session-outcomes session)) outcome
              (gethash span-id (%trace-session-sequences session)) sequence)
        (when root-p
          (setf (%trace-session-root-span session) span
                (%trace-session-root-span-id session) span-id))
        (unwind-protect
            (observability-kit:call-with-span
             span
             (lambda (active-span)
               (declare (ignore active-span))
               (handler-bind
                   ((error (lambda (condition)
                             (setf outcome
                                   (%trace-check-outcome
                                    (if condition-outcome-function
                                        (funcall condition-outcome-function condition)
                                      :error)))
                             (unless (or (member outcome '(:ok :partial) :test #'eq)
                                         (observability-kit:span-ended-p span))
                               (observability-kit:span-set-status span :error)))))
                 (let ((values (multiple-value-list (funcall thunk))))
                   (setf outcome
                         (%trace-check-outcome
                          (if outcome-function (funcall outcome-function values) :ok)))
                   (unless (or (member outcome '(:ok :partial) :test #'eq)
                               (observability-kit:span-ended-p span))
                     (observability-kit:span-set-status span :error))
                   (values-list values)))))
          (setf (gethash span-id (%trace-session-outcomes session)) outcome)
          (unless (observability-kit:span-ended-p span)
            (unless (member outcome '(:ok :partial) :test #'eq)
              (observability-kit:span-set-status span :error))
            (observability-kit:end-span span)))))))
(defun %command-trace-outcome (values)
  (cond ((and values (eql (first values) 0)) :ok)
        ((and values (eql (first values) 3)) :partial)
        (t :error)))
(defun %call-with-command-trace (command-name trace-requested-p thunk)
  (cond (*trace-session*
         (let ((*trace-envelope-owner-p* nil))
           (%call-with-trace-span :command command-name thunk
                                  :outcome-function #'%command-trace-outcome)))
        ((not trace-requested-p) (funcall thunk))
        (t
         (let* ((session (%make-trace-session))
                (*trace-session* session)
                (*trace-envelope-owner-p* t))
           (%call-with-trace-span :command command-name thunk
                                  :outcome-function #'%command-trace-outcome)))))
(defun %trace-nanoseconds (tick-delta units-per-second)
  (floor (* (max 0 tick-delta) 1000000000) units-per-second))
(defun %trace-project-record (session record root-start)
  (let ((span-id (observability-kit:span-record-span-id record)))
    (multiple-value-bind (kind kind-p) (gethash span-id (%trace-session-kinds session))
      (multiple-value-bind (outcome outcome-p)
          (gethash span-id (%trace-session-outcomes session))
        (multiple-value-bind (sequence sequence-p)
            (gethash span-id (%trace-session-sequences session))
          (unless (and kind-p outcome-p sequence-p)
            (error "Trace metadata is missing for span ~S." span-id))
          (%make-projected-span
           :kit-id span-id
           :parent-kit-id (observability-kit:span-record-parent-span-id record)
           :kind kind
           :name (observability-kit:span-record-name record)
           :start-ns (%trace-nanoseconds
                      (- (observability-kit:span-record-start-monotonic record) root-start)
                      (observability-kit:span-record-monotonic-units-per-second record))
           :duration-ns (%trace-nanoseconds
                         (- (observability-kit:span-record-end-monotonic record)
                            (observability-kit:span-record-start-monotonic record))
                         (observability-kit:span-record-monotonic-units-per-second record))
           :outcome outcome
           :sequence sequence))))))
(defun %trace-projected-span< (left right)
  (or (< (%projected-span-start-ns left) (%projected-span-start-ns right))
      (and (= (%projected-span-start-ns left) (%projected-span-start-ns right))
           (< (%projected-span-sequence left) (%projected-span-sequence right)))))
(defun %trace-projected-parent-id (span id-map)
  (let ((parent-kit-id (%projected-span-parent-kit-id span)))
    (if (null parent-kit-id)
        (aitools.protocol.domain:json-null)
      (multiple-value-bind (parent-id present-p) (gethash parent-kit-id id-map)
        (unless present-p
          (error "Trace parent span ~S is missing from the projection." parent-kit-id))
        parent-id))))
(defun %trace-projected-span-json (span dense-id id-map)
  (aitools.protocol.domain:json-object-from-alist
   (list (cons "span_id" dense-id)
         (cons "parent_span_id" (%trace-projected-parent-id span id-map))
         (cons "kind" (%projected-span-kind span))
         (cons "name" (%projected-span-name span))
         (cons "start_ns" (%projected-span-start-ns span))
         (cons "duration_ns" (%projected-span-duration-ns span))
         (cons "outcome" (%trace-outcome-name (%projected-span-outcome span))))))
(defun %trace-counter-value (snapshots name)
  (let ((snapshot (find name snapshots :key #'observability-kit:metric-snapshot-name :test #'string=)))
    (observability-kit:metric-sample-value
     (first (observability-kit:metric-snapshot-samples snapshot)))))
(defun %trace-metrics-json (session spans root-id)
  (let* ((root (find root-id spans :key #'%projected-span-kit-id :test #'string=))
         (metric-values
          (list (cons "span_count" (length spans))
                (cons "command_count"
                      (count "command" spans :key #'%projected-span-kind :test #'string=))
                (cons "child_process_count"
                      (count "child_process" spans :key #'%projected-span-kind :test #'string=))
                (cons "io_count" (count "io" spans :key #'%projected-span-kind :test #'string=))
                (cons "duration_ns" (%projected-span-duration-ns root))))
         (snapshots nil))
    (dolist (entry metric-values)
      (observability-kit:metric-inc
       (cdr (assoc (car entry) (%trace-session-counters session) :test #'string=))
       (cdr entry)))
    (setf snapshots (observability-kit:metric-snapshot (%trace-session-registry session)))
    (aitools.protocol.domain:json-object-from-alist
     (mapcar (lambda (entry)
               (cons (car entry) (%trace-counter-value snapshots (car entry))))
             metric-values))))
(defun %trace-project-session (session)
  (let* ((root-id (%trace-session-root-span-id session))
         (root-record
          (find root-id (%trace-session-records session)
                :key #'observability-kit:span-record-span-id
                :test #'string=)))
    (unless root-record
      (error "The root trace span has no completed record."))
    (let* ((root-start
            (observability-kit:span-record-start-monotonic root-record))
           (spans
            (mapcar (lambda (record)
                      (%trace-project-record session record root-start))
                    (%trace-session-records session)))
           (ordered (sort spans #'%trace-projected-span<))
           (id-map (make-hash-table :test #'equal))
           (span-json nil))
      (loop for span in ordered
            for dense-id from 0
            do (setf (gethash (%projected-span-kit-id span) id-map) dense-id))
      (setf span-json
            (loop for span in ordered
                  for dense-id from 0
                  collect (%trace-projected-span-json span dense-id id-map)))
      (aitools.protocol.domain:json-object-from-alist
       (list
        (cons "trace"
              (aitools.protocol.domain:json-object-from-alist
               (list (cons "spans" span-json)
                     (cons "metrics"
                           (%trace-metrics-json session ordered root-id))))))))))
(defun %trace-envelope-meta (outcome)
  (let ((session *trace-session*))
    (when (and session *trace-envelope-owner-p*)
      (%trace-check-outcome outcome)
      (or (%trace-session-meta session)
          (let ((root (%trace-session-root-span session)))
            (unless root
              (error "The owner trace session has no root span."))
            (setf (gethash (%trace-session-root-span-id session)
                           (%trace-session-outcomes session))
                  outcome)
            (unless (observability-kit:span-ended-p root)
              (unless (member outcome '(:ok :partial) :test #'eq)
                (observability-kit:span-set-status root :error))
              (observability-kit:end-span root))
            (setf (%trace-session-meta session)
                  (%trace-project-session session)))))))
