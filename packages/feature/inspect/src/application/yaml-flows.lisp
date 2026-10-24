;;;; packages/feature/inspect/src/application/yaml-flows.lisp
(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (yaml-get-flow ports path pointer
                &key root tx lock-timeout (max-bytes "16KiB") keys raw
                on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (aitools.kernel.domain:flow-call
  (json-get-flow ports path pointer :root root :tx tx
                 :lock-timeout lock-timeout :format :yaml
                 :max-bytes max-bytes :keys keys :raw raw)
  (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
  (:on-partial (fields)
    (aitools.kernel.domain:flow-continue :on-partial fields))
  (:on-error (code message &key candidates diagnostics conflicts repairs)
    (aitools.kernel.domain:flow-continue
     :on-error code message :candidates candidates :diagnostics diagnostics
     :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow
 (yaml-select-flow ports path pointer
                   &key root tx lock-timeout where pick sort-by desc
                   (output "items") (limit 50)
                   on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (aitools.kernel.domain:flow-call
  (json-select-flow ports path pointer :root root :tx tx
                    :lock-timeout lock-timeout :format :yaml
                    :where where :pick pick :sort-by sort-by :desc desc
                    :output output :limit limit)
  (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
  (:on-partial (fields)
    (aitools.kernel.domain:flow-continue :on-partial fields))
  (:on-error (code message &key candidates diagnostics conflicts repairs)
    (aitools.kernel.domain:flow-continue
     :on-error code message :candidates candidates :diagnostics diagnostics
     :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow
 (yaml-diff-flow ports a b &key root tx lock-timeout (limit 100)
                 on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (aitools.kernel.domain:flow-call
  (json-diff-flow ports a b :root root :tx tx :lock-timeout lock-timeout
                  :format :yaml :limit limit)
  (:on-ok (fields) (aitools.kernel.domain:flow-continue :on-ok fields))
  (:on-partial (fields)
    (aitools.kernel.domain:flow-continue :on-partial fields))
  (:on-error (code message &key candidates diagnostics conflicts repairs)
    (aitools.kernel.domain:flow-continue
     :on-error code message :candidates candidates :diagnostics diagnostics
     :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow
  (parse-yaml-write-document/k text
                               &key max-input-length max-depth on-value on-error)
  "Continue with compact JSON text and comment count, or a public error code and message."
  (declare (type function on-value on-error))
  (handler-case
      (multiple-value-bind (value comment-count)
          (parse-yaml-write-document
           text :max-input-length max-input-length :max-depth max-depth)
        (aitools.kernel.domain:flow-continue
          :on-value (render-json value) comment-count))
    (yaml-unsupported-value (condition)
      (aitools.kernel.domain:flow-continue
        :on-error "input.unsupported-value"
        (format nil "YAML value at JSON Pointer ~A uses unsupported ~A."
                (yaml-unsupported-pointer condition)
                (yaml-unsupported-reason condition))))
    (yaml-syntax-error ()
      (aitools.kernel.domain:flow-continue
        :on-error "input.syntax-error" "YAML syntax error."))))
