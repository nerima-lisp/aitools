(in-package #:aitools.org.infrastructure)

(defun make-production-org-ports (&key workspace-host text-source &allow-other-keys)
  (aitools.org.application:make-org-ports
   :workspace-host workspace-host
   :text-source text-source
   :max-input-bytes
   (org-kit:limit-value *org-kit-limits* :max-input-length)
   :parse-org #'parse-org-octets
   :outline-org #'outline-org-document
   :query-org #'query-org-document
   :export-org #'export-org-document))
