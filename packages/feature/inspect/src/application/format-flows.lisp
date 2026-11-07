(in-package #:aitools.inspect.application)

(defun
 %format-group (format)
 (declare (ignore format))
 "doc")

(defun %document-format (path override)
  (let ((extension (or override (pathname-type path))))
    (when extension
      (cond ((string-equal extension "json") :json)
            ((or (string-equal extension "yaml")
                 (string-equal extension "yml"))
             :yaml)
            ((string-equal extension "toml") :toml)))))

(aitools.kernel.domain:define-flow
 (doc-inspect-flow flow ports action paths arguments format options
                   &key on-ok on-partial on-error)
 (declare (type function flow on-ok on-partial on-error))
 (let* ((formats (mapcar (lambda (path)
                           (%document-format path format))
                         paths))
        (unknown (position nil formats)))
   (if unknown
       (let ((path (nth unknown paths)))
         (aitools.kernel.domain:flow-continue
          :on-error
          "argument.invalid"
          (format nil
                  "Cannot determine the document format for ~A; pass --format json, yaml, or toml."
                  path)
          :repairs
          (list
           (repair "specify-format"
                   "Pass --format with the format that matches the document."
                   (apply #'aitools.protocol.domain:command-line
                          (append (list "aitools" "doc" action)
                                  arguments
                                  (list "--format" "<json|yaml|toml>")))))))
       (apply flow
              (append
               (list ports)
               arguments
               (if (rest formats)
                   (list :format-a (first formats)
                         :format-b (second formats))
                   (list :format (first formats)))
               options
               (list :on-ok on-ok
                     :on-partial on-partial
                     :on-error on-error))))))

(progn
  (defmacro %call-document-reader (call)
    `(aitools.kernel.domain:flow-call
      ,call
      (:on-document (document)
        (aitools.kernel.domain:flow-continue :on-document document))
      (:on-error (code message &key candidates diagnostics conflicts repairs)
        (aitools.kernel.domain:flow-continue
         :on-error code message
         :candidates candidates :diagnostics diagnostics
         :conflicts conflicts :repairs repairs))))

  (aitools.kernel.domain:define-flow
   (%inspect-document context target format action &key on-document on-error)
   (declare (type function on-document on-error))
   (case format
     (:json
      (%call-document-reader
       (%json-document context target)))
     (:yaml
      (%call-document-reader
       (%yaml-document context target action)))
     (:toml
      (%call-document-reader
       (%toml-document context target action))))))

(aitools.kernel.domain:define-flow
 (%call-with-format-file ports path format action
                         &key root tx lock-timeout record
                         on-document on-error)
 (declare (type function on-document on-error))
 (labels ((report-error (code message
                              &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates :diagnostics diagnostics
             :conflicts conflicts :repairs repairs)))
   (aitools.kernel.domain:flow-call
    (call-with-inspect-file/k
     ports path :root root :tx tx :lock-timeout lock-timeout :record record)
    (:on-file (context target)
      (aitools.kernel.domain:flow-call
       (%inspect-document context target format action)
       (:on-document (document)
         (aitools.kernel.domain:flow-continue
          :on-document context target document))
       (:on-error (code message
                        &key candidates diagnostics conflicts repairs)
         (report-error code message
                       :candidates candidates :diagnostics diagnostics
                       :conflicts conflicts :repairs repairs))))
    (:on-error (code message
                     &key candidates diagnostics conflicts repairs)
      (report-error code message
                    :candidates candidates :diagnostics diagnostics
                    :conflicts conflicts :repairs repairs)))))
