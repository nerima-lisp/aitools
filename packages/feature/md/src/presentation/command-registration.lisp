(in-package #:aitools.md.presentation)

(defun register-md-commands (registry ports)
  (aitools.protocol.application:register-command-declarations registry
                                                              (list
                                                               (aitools.protocol.application:define-command
                                                                "md.headings"
                                                                (:group "md"
                                                                        :command-builder
                                                                        #'%headings-command
                                                                        :schema
                                                                        (%schema "md.headings")))
                                                               (aitools.protocol.application:define-command "md.read"
                                                                                                            (:group "md"
                                                                                                                    :command-builder
                                                                                                                    #'%read-command
                                                                                                                    :schema
                                                                                                                    (%schema
                                                                                                                     "md.read")))
                                                               (aitools.protocol.application:define-command "md.replace"
                                                                                                            (:group "md"
                                                                                                                    :command-builder
                                                                                                                    #'%replace-command
                                                                                                                    :schema
                                                                                                                    (%schema
                                                                                                                     "md.replace"))))
                                                              ports)
  registry)
