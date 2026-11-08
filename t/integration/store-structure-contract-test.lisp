(in-package #:aitools.integration.structure-test)

(describe "aitools structure test (store lock kit dependency)"
  (it "allows application boundary objects and keeps retry execution in infrastructure"
    (expect (symbol-allowed-p "store" :application "RESILIENCE-KIT" "MAKE-RETRY-POLICY")
            :to-be-truthy)
    (dolist (layer (list :application :infrastructure))
      (expect (member "CL-BOUNDARY-KIT"
                      (allowed-package-names "store" layer :core)
                      :test (function string=))
              :to-be-truthy))
    (expect (member "CL-BOUNDARY-KIT"
                    (allowed-package-names "store" :domain :core)
                    :test (function string=))
            :to-equal nil)
    (dolist (reference (list (list "RESILIENCE-KIT" "CALL-WITH-RETRY")
                             (list "RESILIENCE-KIT" "RETRY-EXHAUSTED")))
      (destructuring-bind (package symbol) reference
        (expect (symbol-allowed-p "store" :application package symbol) :to-equal nil)
        (expect (symbol-allowed-p "store" :domain package symbol) :to-equal nil)
        (expect (symbol-allowed-p "store" :infrastructure package symbol) :to-be-truthy)
        (expect (symbol-allowed-p "protocol" :application package symbol) :to-equal nil)))
    (expect (symbol-allowed-p "store" :application "OTHER-KIT" "CALL-WITH-RETRY")
            :to-equal nil)
    (expect (symbol-allowed-p "store" :application "RESILIENCE-KIT" "UNLISTED-RETRY")
            :to-equal nil))

  (it "blocks effectful boundary constructors from application"
    (dolist (reference (list (list "CL-BOUNDARY-KIT" "MAKE-CLOCK")
                             (list "CL-BOUNDARY-KIT" "MAKE-SLEEPER")))
      (destructuring-bind (package symbol) reference
        (expect (symbol-allowed-p "store" :application package symbol) :to-equal nil)
        (expect (symbol-allowed-p "store" :domain package symbol) :to-equal nil)
        (expect (application-effectful-boundary-symbol-p "store" :application package symbol)
                :to-be-truthy)))
    (expect (application-effectful-boundary-symbol-p
             "store" :application "CL-BOUNDARY-KIT" "CLOCK-NOW")
            :to-equal nil)
    (dolist (symbol *application-effectful-boundary-constructors*)
      (expect (application-effectful-boundary-symbol-p
               "store" :application "CL-BOUNDARY-KIT" symbol)
              :to-be-truthy))
    (let* ((relative "packages/feature/env/src/infrastructure/production-ports.lisp")
           (pathname (merge-pathnames relative (repository-root)))
           (violations (scan-file pathname relative "store" :application :core nil)))
      (dolist (symbol (list "MAKE-CLOCK" "MAKE-ENVIRONMENT"
                            "MAKE-FILESYSTEM" "MAKE-HOST-INFO"))
        (let ((expected (format nil
                                "CL-BOUNDARY-KIT:~A, which store/application may not depend on"
                                symbol)))
          (expect (not (null (some (lambda (violation) (search expected violation))
                                   violations)))
                  :to-equal t))))
    (uiop:with-temporary-file (:pathname pathname :type "lisp"
                               :stream stream :direction :output)
      (format stream
              "(defpackage #:aitools.store.application (:use #:cl #:cl-boundary-kit) (:import-from #:cl-boundary-kit #:make-filesystem))~%(in-package #:aitools.store.application)~%")
      (finish-output stream)
      (expect (scan-file pathname "fixture.lisp" "store" :application :core nil)
              :to-equal
              (list "fixture.lisp:1: :use CL-BOUNDARY-KIT imports effectful constructors into store/application"
                    "fixture.lisp:1: :import-from CL-BOUNDARY-KIT:MAKE-FILESYSTEM, which store/application may not depend on")))))
