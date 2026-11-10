(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data* '(:context "search"
                                                 :symbol-allowances
                                                 (("search" (:composition)
                                                            "CL-BOUNDARY-KIT"
                                                            "MAKE-BOUNDARY-CONTEXT"
                                                            "Creates the search boundary context in composition.")
                                                  ("search" (:composition)
                                                            "CL-BOUNDARY-KIT"
                                                            "BOUNDARY-CONTEXT-WITH"
                                                            "Registers search capabilities in the composition context.")
                                                  ("search" (:infrastructure)
                                                            "AITOOLS.KERNEL.DOMAIN"
                                                            "UNIVERSAL-TIME-TO-UNIX-SECONDS"
                                                            "Converts host universal time for the search Unix clock capability."))))
