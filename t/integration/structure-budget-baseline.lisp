(in-package #:aitools.integration.structure-test)

(defparameter *line-budget-limit*
  250)

(defparameter *line-budget-ratchet*
  '())
(defparameter *structure-budget-context-data*
  (%load-context-structure-data-files
   (merge-pathnames
    "t/integration/structure-budget-baselines/"
    (asdf:system-source-directory "aitools"))))

(defparameter *line-budget-generated-exemptions*
  (loop for context-data in *structure-budget-context-data*
        append (getf context-data :generated-exemptions)))

(defparameter *literal-table-limit*
  16)

(defparameter *literal-table-limit-rationale*
  "Baseline 1b20c3abdea1d95a049cdb0481afcfd72072b2a4 measured 289 source files: 13 is the largest quoted collection below threshold 16, and 19 is the smallest above it.")

(defparameter *literal-table-ratchet*
  '())

(defparameter *complex-function-ratchet* '())
(defparameter *complex-function-baseline*
  ;; Baseline revision 1b20c3abdea1d95a049cdb0481afcfd72072b2a4; entries exceeded depth 4 under the current control-depth measurement.
  (loop for context-data in *structure-budget-context-data*
        append (getf context-data :complex-functions)))

