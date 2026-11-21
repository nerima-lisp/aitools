(in-package #:aitools.org.infrastructure)

(defun make-production-org-capabilities
    (&key workspace-host text-source open-store &allow-other-keys)
  (list (cons :org-workspace-host workspace-host)
        (cons :org-text-source text-source)
        (cons :org-max-input-bytes
              (org-kit:limit-value *org-kit-limits* :max-input-length))
        (cons :org-open-store open-store)
        (cons :org-parse (function parse-org-octets))
        (cons :org-outline (function outline-org-document))
        (cons :org-query (function query-org-document))
        (cons :org-export (function export-org-document))
        (cons :org-write (function write-org-octets))
        (cons :org-edit (function edit-org-document))
        (cons :org-agenda (function agenda-org-document))
        (cons :org-clock-report (function clock-report-org-document))))
