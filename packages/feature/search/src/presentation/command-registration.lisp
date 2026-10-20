(in-package #:aitools.search.presentation)

(defun register-search-commands (registry ports)
  "Register `search`, `find`, `overview`, and `code outline|defs|refs` on
REGISTRY. PORTS is the AITOOLS.SEARCH.APPLICATION:SEARCH-PORTS the
composition root built."
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
     "search"
     (:command-builder #'%search-command
      :schema (%schema "search" :scan-args t)))
    (aitools.protocol.application:define-command
     "find"
     (:command-builder #'%find-command
      :schema (%schema "find" :scan-args t)))
    (aitools.protocol.application:define-command
     "overview"
     (:command-builder #'%overview-command
      :schema (%schema "overview")))
    (aitools.protocol.application:define-command
     "code.outline"
     (:group "code"
      :command-builder #'%outline-command
      :schema (%schema "code.outline")))
    (aitools.protocol.application:define-command
     "code.defs"
     (:group "code"
      :command-builder #'%defs-command
      :schema (%schema "code.defs")))
    (aitools.protocol.application:define-command
     "code.refs"
     (:group "code"
      :command-builder #'%refs-command
      :schema (%schema "code.refs"))))
   ports)
  registry)
