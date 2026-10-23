(in-package #:aitools.e2e.test)

(defun %trace-store-io-names ()
  '("store.lstat" "store.read-file" "store.create-file" "store.append-file"
    "store.rename" "store.unlink" "store.rmdir" "store.mkdir" "store.chmod"
    "store.chmod-directory-identity" "store.sync-directory" "store.symlink"
    "store.set-mtime" "store.list-directory" "store.try-lock" "store.unlock"))

(defun %trace-object-keys (object)
  (loop for key being the hash-keys of object collect key))

(defun %expect-store-trace (envelope &rest private-markers)
  (let* ((trace (jget envelope "meta" "trace"))
         (spans (jlist (jget trace "spans")))
         (metrics (jget trace "metrics"))
         (ids (loop for span in spans collect (jget span "span_id")))
         (root (find-if (lambda (span)
                          (and (string= (jget span "kind") "command")
                               (null (jget span "parent_span_id"))))
                        spans))
         (serialized (with-output-to-string (stream)
                       (prin1 trace stream))))
    (expect (equal (sort (%trace-object-keys trace) #'string<)
                   '("metrics" "spans"))
            :to-be t)
    (expect (equal (sort (%trace-object-keys metrics) #'string<)
                   '("child_process_count" "command_count" "duration_ns"
                     "io_count" "span_count"))
            :to-be t)
    (expect ids :to-equal (loop for id below (length spans) collect id))
    (expect root :to-be-truthy)
    (expect (= 1 (count-if (lambda (span)
                             (and (string= (jget span "kind") "command")
                                  (null (jget span "parent_span_id"))))
                           spans))
            :to-be t)
    (expect (equal (sort (%trace-object-keys root) #'string<)
                   '("duration_ns" "kind" "name" "outcome" "parent_span_id"
                     "span_id" "start_ns"))
            :to-be t)
    (multiple-value-bind (root-parent root-parent-p)
        (gethash "parent_span_id" root)
      (expect root-parent-p :to-be t)
      (expect root-parent :to-be nil))
    (expect (loop for index from 1 below (length spans)
                  for previous = (nth (1- index) spans)
                  for current = (nth index spans)
                  always
                  (or (< (jget previous "start_ns") (jget current "start_ns"))
                      (and (= (jget previous "start_ns") (jget current "start_ns"))
                           (< (jget previous "span_id") (jget current "span_id")))))
            :to-be t)
    (dolist (span spans)
      (let ((kind (jget span "kind"))
            (span-id (jget span "span_id")))
        (expect (equal (sort (%trace-object-keys span) #'string<)
                       '("duration_ns" "kind" "name" "outcome" "parent_span_id"
                         "span_id" "start_ns"))
                :to-be t)
        (expect (member kind '("command" "io") :test #'string=) :to-be-truthy)
        (expect (stringp (jget span "name")) :to-be t)
        (when (string= kind "io")
          (expect (member (jget span "name") (%trace-store-io-names) :test #'string=)
                  :to-be-truthy))
        (expect (member (jget span "outcome")
                        '("ok" "partial" "error" "timeout" "unavailable")
                        :test #'string=)
                :to-be-truthy)
        (expect (and (integerp span-id)
                     (integerp (jget span "start_ns"))
                     (<= 0 (jget span "start_ns"))
                     (integerp (jget span "duration_ns"))
                     (<= 0 (jget span "duration_ns")))
                :to-be t)
        (multiple-value-bind (parent parent-present-p)
            (gethash "parent_span_id" span)
          (expect parent-present-p :to-be t)
          (expect (if (= span-id (jget root "span_id"))
                      (null parent)
                      (eql parent (jget root "span_id")))
                  :to-be t)
          (when parent
            (expect (member parent ids) :to-be-truthy)))))
    (expect (gethash "span_count" metrics) :to-equal (length spans))
    (expect (gethash "command_count" metrics)
            :to-equal (count "command" spans :key (lambda (span) (jget span "kind"))
                                                :test #'string=))
    (expect (gethash "child_process_count" metrics) :to-equal 0)
    (expect (gethash "io_count" metrics)
            :to-equal (count "io" spans :key (lambda (span) (jget span "kind"))
                                         :test #'string=))
    (expect (gethash "duration_ns" metrics) :to-equal (jget root "duration_ns"))
    (dolist (marker private-markers)
      (expect (search marker serialized) :to-be nil))
    trace))

(describe "aitools store IO trace spans (e2e)"
  (it "traces store reads, writes, and journal appends with fixed span names"
    (:tags (quote (:trace-positive)))
    (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
            (and directory (plusp (length directory))))
      (cl-weave:skip "trace specs require a binary with --trace"))
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (put ws "private-read-path-marker.txt" "private-read-body-marker")
       (let* ((read-result
                (aitools ws '("--trace" "read" "private-read-path-marker.txt")
                         :expect-trace t))
              (read-trace
                (%expect-store-trace (aitools-envelope read-result)
                                     "private-read-path-marker.txt"
                                     "private-read-body-marker"))
              (first-write
                (aitools ws '("--trace" "write" "private-write-path-marker.txt" "--stdin")
                         :stdin "private-write-body-marker"
                         :expect-trace t))
              (first-write-trace
                (%expect-store-trace (aitools-envelope first-write)
                                     "private-write-path-marker.txt"
                                     "private-write-body-marker")))
         (expect (aitools-exit-code read-result) :to-be 0)
         (expect (find "store.read-file" (jlist (jget read-trace "spans")) :key (lambda (span) (jget span "name")) :test (function string=)) :to-be nil)
         (expect (aitools-exit-code first-write) :to-be 0)
         (%expect-store-span first-write-trace "store.create-file")
         (%expect-store-span first-write-trace "store.rename")
         (let* ((second-write
                  (aitools ws '("--trace" "write" "second-write-path-marker.txt" "--stdin")
                           :stdin "second-write-body-marker"
                           :expect-trace t))
                (trace (%expect-store-trace (aitools-envelope second-write)
                                            "second-write-path-marker.txt"
                                            "second-write-body-marker")))
           (expect (aitools-exit-code second-write) :to-be 0)
           (%expect-store-span trace "store.append-file")))))))

(describe "aitools store transaction trace spans (e2e)"
  (it "traces tx stage, read, and commit through the same store IO names"
    (:tags (quote (:trace-positive)))
    (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
            (and directory (plusp (length directory))))
      (cl-weave:skip "trace specs require a binary with --trace"))
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let* ((begin-result (aitools ws (list "--trace" "tx" "begin") :expect-trace t))
              (begin-envelope (aitools-envelope begin-result))
              (begin-trace (%expect-store-trace begin-envelope))
              (tx (jget begin-envelope "tx")))
         (expect (aitools-exit-code begin-result) :to-be 0)
         (expect tx :to-be-truthy)
         (%expect-store-span begin-trace "store.create-file")
         (let* ((stage-result
                  (aitools ws (list "--trace" "write" "tx-private-path-marker.txt"
                                    "--stdin" "--tx" tx)
                           :stdin "tx-private-body-marker"
                           :expect-trace t))
                (stage-envelope (aitools-envelope stage-result))
                (stage-trace (%expect-store-trace stage-envelope
                                                  "tx-private-path-marker.txt"
                                                  "tx-private-body-marker")))
           (expect (aitools-exit-code stage-result) :to-be 0)
           (expect (jget stage-envelope "tx_op") :to-equal 1)
           (%expect-store-span stage-trace "store.create-file"))
         (let* ((read-result
                  (aitools ws (list "--trace" "read" "tx-private-path-marker.txt"
                                    "--tx" tx)
                           :expect-trace t))
                (read-trace (%expect-store-trace (aitools-envelope read-result)
                                                 "tx-private-path-marker.txt"
                                                 "tx-private-body-marker")))
           (expect (aitools-exit-code read-result) :to-be 0)
           (%expect-store-span read-trace "store.read-file"))
         (let* ((commit-result (aitools ws (list "--trace" "tx" "commit" tx)
                                        :expect-trace t))
                (commit-envelope (aitools-envelope commit-result))
                (commit-trace (%expect-store-trace commit-envelope
                                                   "tx-private-path-marker.txt"
                                                   "tx-private-body-marker")))
           (expect (aitools-exit-code commit-result) :to-be 0)
           (expect (file-text ws "tx-private-path-marker.txt")
                   :to-equal "tx-private-body-marker")
           (%expect-store-span commit-trace "store.rename")
           (%expect-store-span commit-trace "store.create-file")))))))

(describe "aitools store recovery rewrite trace spans (e2e)"
          (it "traces the journal rewrite when startup recovery rolls forward an intent"
              (:tags (quote (:trace-positive)))
              (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
                      (and directory (plusp (length directory))))
                (cl-weave:skip "trace specs require a binary with --trace"))
              (aitools-binary)
              (call-with-workspace
               (lambda (ws)
                 (let ((seed-result
                        (aitools ws '("--trace" "write" "seed-path-marker.txt" "--stdin")
                                 :stdin "seed-body-marker"
                                 :expect-trace t)))
                   (expect (aitools-exit-code seed-result) :to-be 0)
                   (%expect-store-trace (aitools-envelope seed-result)
                                        "seed-path-marker.txt" "seed-body-marker"))
                 (let* ((root (native (workspace-root ws)))
                        (store-home (aitools.store.domain:state-home
                                     (native (workspace-state ws)) "/"))
                        (store (aitools.store.application:make-store
                                (aitools.store.infrastructure:make-posix-store-io :root root)
                                root store-home))
                        (fault-result
                         (aitools.store.test-support:call-with-fault-at
                          :after-apply
                          (lambda ()
                            (aitools.store.application:commit-changes/k
                             store '("test" "write")
                             (lambda (commit reject)
                               (declare (ignore reject))
                               (funcall commit
                                        (list
                                         (aitools.store.domain:write-file-request
                                          "recovery-private-path-marker.txt"
                                          (aitools.store.domain:string-octets
                                           "recovery-private-body-marker")))))
                             :on-committed
                             (lambda (op-id results)
                               (declare (ignore op-id results))
                               :committed)
                             :on-rejected
                             (lambda (code message &rest details)
                               (declare (ignore details))
                               (list :rejected code message))
                             :on-busy (lambda () :busy))
                            :mode :throw))))
                   (expect fault-result :to-be t)
                   (let* ((recovered
                           (aitools ws '("--trace" "read" "recovery-private-path-marker.txt")
                                    :expect-trace t))
                          (envelope (aitools-envelope recovered))
                          (trace (%expect-store-trace envelope
                                                      "recovery-private-path-marker.txt"
                                                      "recovery-private-body-marker")))
                     (expect (aitools-exit-code recovered) :to-be 0)
                     (expect (jlist (jget envelope "recovered")) :to-be-truthy)
                     (%expect-store-span trace "store.create-file")
                     (%expect-store-span trace "store.rename")))))))

(defun %expect-store-span (trace name)
  (expect (find name (jlist (jget trace "spans"))
                :key (lambda (span) (jget span "name"))
                :test #'string=)
          :to-be-truthy))
