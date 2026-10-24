(in-package #:aitools.org.application)

(aitools.kernel.domain:define-flow (%org-write-flow ports path command operation options &key root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial)
                                   (declare (ignore expect-count expect-hash on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-result ports path command operation options :root root :lock-timeout lock-timeout :dry-run dry-run :tx tx :display-argv display-argv)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-todo-set/k ports &key path match state note at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial) (declare (ignore on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.todo.set"
                                                     :todo-set
                                                     (list :match match :state state :note note :at at :tz tz :dst-policy dst-policy)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv
                                                     :expect-count
                                                     expect-count
                                                     :expect-hash
                                                     expect-hash)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-property-set/k ports &key path match name value at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial) (declare (ignore on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.property.set"
                                                     :property-set
                                                     (list :match match :name name :value value :at at :tz tz :dst-policy dst-policy)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv
                                                     :expect-count
                                                     expect-count
                                                     :expect-hash
                                                     expect-hash)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-property-delete/k ports &key path match name at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial) (declare (ignore on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.property.delete"
                                                     :property-delete
                                                     (list :match match :name name :at at :tz tz :dst-policy dst-policy)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv
                                                     :expect-count
                                                     expect-count
                                                     :expect-hash
                                                     expect-hash)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-planning-set/k ports &key path match kind timestamp at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial) (declare (ignore on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.planning.set"
                                                     :planning-set
                                                     (list :match match :kind kind :timestamp timestamp :at at :tz tz :dst-policy dst-policy)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv
                                                     :expect-count
                                                     expect-count
                                                     :expect-hash
                                                     expect-hash)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-planning-delete/k ports &key path match kind at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial) (declare (ignore on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.planning.delete"
                                                     :planning-delete
                                                     (list :match match :kind kind :at at :tz tz :dst-policy dst-policy)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv
                                                     :expect-count
                                                     expect-count
                                                     :expect-hash
                                                     expect-hash)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-clock-in/k ports &key path match at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial) (declare (ignore on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.clock.in"
                                                     :clock-in
                                                     (list :match match :at at :tz tz :dst-policy dst-policy)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv
                                                     :expect-count
                                                     expect-count
                                                     :expect-hash
                                                     expect-hash)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-clock-out/k ports &key path match at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial) (declare (ignore on-partial))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.clock.out"
                                                     :clock-out
                                                     (list :match match :at at :tz tz :dst-policy dst-policy)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv
                                                     :expect-count
                                                     expect-count
                                                     :expect-hash
                                                     expect-hash)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-table-align/k ports &key path table root lock-timeout dry-run tx display-argv on-ok on-error on-partial expect-count expect-hash) (declare (ignore on-partial expect-count expect-hash))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.table.align"
                                                     :table-align
                                                     (list :table table)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-table-sort/k ports &key path table column type order start end root lock-timeout dry-run tx display-argv on-ok on-error on-partial expect-count expect-hash) (declare (ignore on-partial expect-count expect-hash))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.table.sort"
                                                     :table-sort
                                                     (list :table table :column column :type type :order order :start start :end end)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow (org-table-recalculate/k ports &key path table root lock-timeout dry-run tx display-argv on-ok on-error on-partial expect-count expect-hash) (declare (ignore on-partial expect-count expect-hash))
                                   (aitools.kernel.domain:flow-call
                                    (%org-write-flow ports
                                                     path
                                                     "org.table.recalculate"
                                                     :table-recalculate
                                                     (list :table table)
                                                     :root
                                                     root
                                                     :lock-timeout
                                                     lock-timeout
                                                     :dry-run
                                                     dry-run
                                                     :tx
                                                     tx
                                                     :display-argv
                                                     display-argv)
                                    (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
                                    (:on-error (code message &key candidates diagnostics conflicts repairs)
                                               (aitools.kernel.domain:flow-continue :on-error code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs))))
