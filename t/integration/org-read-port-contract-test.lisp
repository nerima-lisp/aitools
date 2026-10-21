(in-package #:aitools.integration.org-dispatch-test)

(defun %org-flow-result (flow ports &rest arguments)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append (list ports) arguments continuations)))))

(defun %empty-org-ports ()
  (aitools.org.application:make-org-ports :max-input-bytes 0))

(defun %org-ports-without-operation (missing)
  (let* ((host (aitools.workspace.infrastructure:make-host-workspace-host))
         (source (aitools.text.infrastructure:make-host-text-source))
         (production (aitools.org.infrastructure:make-production-org-ports
                      :workspace-host host :text-source source)))
    (aitools.org.application:make-org-ports
     :workspace-host host
     :text-source source
     :max-input-bytes (aitools.org.application:org-ports-max-input-bytes production)
     :parse-org (aitools.org.application:org-ports-parse-org production)
     :outline-org (unless (eq missing :outline)
                    (aitools.org.application:org-ports-outline-org production))
     :query-org (unless (eq missing :query)
                  (aitools.org.application:org-ports-query-org production))
     :export-org (unless (eq missing :export)
                   (aitools.org.application:org-ports-export-org production)))))

(defun %expect-org-flow-error (result code message)
  (expect (aitools.protocol.application:command-result-kind result) :to-be :error)
  (let ((fields (aitools.protocol.application:command-result-fields result)))
    (expect (getf fields :code) :to-equal code)
    (expect (getf fields :message) :to-equal message)
    fields))

(describe "Org application read flow contracts"
  (it "validates command arguments before requiring ports or opening a file"
    (let ((ports (%empty-org-ports)))
      (%expect-org-flow-error
       (%org-flow-result (function aitools.org.application:org-outline/k) ports
                         :path nil)
       "argument.invalid" "an Org file path is required")
      (%expect-org-flow-error
       (%org-flow-result (function aitools.org.application:org-outline/k) ports
                         :path "")
       "argument.invalid" "an Org file path is required")
      (%expect-org-flow-error
       (%org-flow-result (function aitools.org.application:org-outline/k) ports
                         :path "missing.org" :limit -1)
       "argument.invalid" "outline limit must be a non-negative integer")
      (%expect-org-flow-error
       (%org-flow-result (function aitools.org.application:org-query/k) ports
                         :path "missing.org" :match "TODO" :at "" :tz "UTC")
       "argument.invalid" "--match, --at, and --tz are required")
      (%expect-org-flow-error
       (%org-flow-result (function aitools.org.application:org-export/k) ports
                         :path "missing.org" :backend :pdf)
       "argument.invalid" "--backend must be html, gfm, or ascii")))
  (it "accepts all keyword DST policies through the query flow"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (format nil "~A/query.org" root))
             (source (format nil "#+TODO: TODO | DONE~%* TODO Task~%"))
             (ports (%org-ports-without-operation :none)))
        (aitools.test.support:write-text path source)
        (dolist (policy (list :compatible :earlier :later :strict))
          (let* ((result (%org-flow-result
                         (function aitools.org.application:org-query/k) ports
                         :path "query.org" :root root :match "LEVEL=1"
                         :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy policy))
                 (fields (aitools.protocol.application:command-result-fields result)))
            (expect (aitools.protocol.application:command-result-kind result) :to-be :ok)
            (expect (cdr (assoc "count" fields :test (function string=)))
                    :to-be 1)))
        (%expect-org-flow-error
         (%org-flow-result (function aitools.org.application:org-query/k) ports
                           :path "query.org" :root root :match "LEVEL=1"
                           :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy :unknown)
         "argument.invalid"
         "query requires valid --at, --tz, and --dst-policy"))))
  (it "validates query adapter inputs at its own boundary"
    (multiple-value-bind (document parse-failure)
        (aitools.org.infrastructure:parse-org-octets
         (sb-ext:string-to-octets (format nil "* Task~%")
                                  :external-format :utf-8))
      (expect (null parse-failure) :to-be t)
      (expect (aitools.org.domain:org-document-p document) :to-be t)
      (labels ((expect-failure (match at tz dst-policy message)
                 (multiple-value-bind (matches failure)
                     (aitools.org.infrastructure::query-org-document
                      document match at tz dst-policy)
                   (expect matches :to-be nil)
                   (expect (aitools.org.domain:org-failure-kind failure)
                           :to-be :argument)
                   (expect (aitools.org.domain:org-failure-message failure)
                           :to-equal message))))
        (expect-failure nil "2026-10-19T09:00:00" "UTC" "compatible"
                        "--match is required")
        (expect-failure "" "2026-10-19T09:00:00" "UTC" "compatible"
                        "--match is required")
        (expect-failure "LEVEL=1" nil "UTC" "compatible"
                        "query requires valid --at, --tz, and --dst-policy")
        (expect-failure "LEVEL=1" "2026-10-19T09:00:00" nil "compatible"
                        "query requires valid --at, --tz, and --dst-policy")
        (expect-failure "LEVEL=1" "2026-10-19T09:00:00" "" "compatible"
                        "query requires valid --at, --tz, and --dst-policy"))))
  (it "returns internal errors for each missing required or operation port"
    (dolist (ports
             (list (aitools.org.application:make-org-ports
                    :workspace-host nil :text-source t :max-input-bytes 0
                    :parse-org #'identity)
                   (aitools.org.application:make-org-ports
                    :workspace-host t :text-source nil :max-input-bytes 0
                    :parse-org #'identity)
                   (aitools.org.application:make-org-ports
                    :workspace-host t :text-source t :max-input-bytes 0)))
      (%expect-org-flow-error
       (%org-flow-result #'aitools.org.application:org-outline/k ports
                         :path "missing.org")
       "internal.unexpected" "the org context is missing a required port"))
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate 'string root "/entry.org"))
             (source (format nil "* Entry~%")))
        (aitools.test.support:write-text path source)
        (dolist (operation '(:outline :query :export))
          (let* ((ports (%org-ports-without-operation operation))
                 (arguments
                   (case operation
                     (:outline (list :path "entry.org" :root root))
                     (:query (list :path "entry.org" :root root :match "TODO"
                                   :at "2026-10-19T09:00:00" :tz "UTC"))
                     (:export (list :path "entry.org" :root root :backend :html))))
                 (flow (ecase operation
                         (:outline #'aitools.org.application:org-outline/k)
                         (:query #'aitools.org.application:org-query/k)
                         (:export #'aitools.org.application:org-export/k)))
                 (message (ecase operation
                            (:outline "the org context has no outline port")
                            (:query "the org context has no query port")
                            (:export "the org context has no export port")))
                 (command (ecase operation
                            (:outline "aitools schema org outline")
                            (:query "aitools schema org query")
                            (:export "aitools schema org export")))
                 (fields (apply #'%expect-org-flow-error
                                (apply #'%org-flow-result flow ports arguments)
                                (list "internal.unexpected" message))))
            (expect (getf (first (getf fields :repairs)) :command)
                    :to-equal command)))
        (expect (aitools.test.support:read-bytes path)
                :to-equalp (sb-ext:string-to-octets source :external-format :utf-8)))))
  (it
  "validates outline and export adapter arguments"
  (multiple-value-bind (document parse-failure)
      (aitools.org.infrastructure:parse-org-octets
       (sb-ext:string-to-octets (format nil "* Task~%")
                                :external-format :utf-8))
    (expect (null parse-failure) :to-be t)
    (multiple-value-bind (entries outline-failure)
        (aitools.org.infrastructure::outline-org-document document -1)
      (expect entries :to-be nil)
      (expect (aitools.org.domain:org-failure-kind outline-failure)
              :to-be :argument)
      (expect (aitools.org.domain:org-failure-message outline-failure)
              :to-equal "outline limit must be a non-negative integer"))
    (multiple-value-bind (content export-failure)
        (aitools.org.infrastructure::export-org-document document :pdf)
      (expect content :to-be nil)
      (expect (aitools.org.domain:org-failure-kind export-failure)
              :to-be :argument)
      (expect (aitools.org.domain:org-failure-message export-failure)
              :to-equal "export backend must be html, gfm, or ascii")))))
