(in-package #:aitools.edit.application)

(defun selector-options (options)
  "The selector keywords of OPTIONS for PARSE-SELECTOR-OPTIONS/K."
  (list :range
        (getf options :range)
        :symbol
        (getf options :symbol)
        :kind
        (getf options :kind)
        :between
        (getf options :between)
        :exclusive
        (getf options :exclusive)
        :match
        (getf options :match)
        :invert
        (getf options :invert)))

(aitools.kernel.domain:define-flow
  (parse-selector/k command options &key extra-exclusive on-selector on-none on-error)
  "The one selector of OPTIONS for COMMAND (a kernel command keyword).
ON-ERROR (code message &key repairs)."
  (apply #'aitools.inspect.application:parse-selector-options/k
         command
         :extra-exclusive
         extra-exclusive
         :on-selector
         (lambda (selector)
           (aitools.kernel.domain:flow-continue
             :on-selector
             selector))
         :on-none
         (lambda ()
           (aitools.kernel.domain:flow-continue
             :on-none))
         :on-invalid
         (lambda (message repairs)
           (aitools.kernel.domain:flow-continue
             :on-error
             "argument.invalid"
             message
             :repairs
             repairs))
         (selector-options options)))

(defun selector-guards (selector path)
  "The guard requirements a SELECTOR brings for a write to PATH."
  (append (and (aitools.kernel.domain:guard-required-p :expect-hash selector) (list (list :expect-hash path)))
          (and (aitools.kernel.domain:guard-required-p :expect-count selector) (list (list :expect-count)))))

(defun content-selector-p (selector)
  (or (null selector) (eq (aitools.kernel.domain:selector-basis selector) :content)))

(aitools.kernel.domain:define-flow
  (resolve-lines/k document selector path &key on-error on-ranges)
  "SELECTOR resolved against DOCUMENT: ON-RANGES (ranges), 1-based
inclusive (start . end) pairs; no-match, ambiguity, and invalid cases go
to ON-ERROR with their candidates."
  (aitools.inspect.application:resolve-selector/k (text-document-lines document)
                                                  selector
                                                  :path
                                                  path
                                                  :on-selected
                                                  (lambda (ranges)
                                                    (aitools.kernel.domain:flow-continue
                                                      :on-ranges
                                                      ranges))
                                                  :on-no-match
                                                  (lambda (candidates)
                                                    (aitools.kernel.domain:flow-continue
                                                      :on-error
                                                      "selection.no-match"
                                                      (format
                                                        nil
                                                        "the selector matches nothing in ~A"
                                                        path)
                                                      :candidates
                                                      candidates))
                                                  :on-ambiguous
                                                  (lambda (candidates)
                                                    (aitools.kernel.domain:flow-continue
                                                      :on-error
                                                      "selection.ambiguous"
                                                      (format
                                                        nil
                                                        "the selector matches ~D places in ~A; it must match one"
                                                        (length candidates)
                                                        path)
                                                      :candidates
                                                      candidates))
                                                  :on-invalid
                                                  (lambda (code message)
                                                    (aitools.kernel.domain:flow-continue
                                                      :on-error
                                                      code
                                                      message))))

(defun lines-candidates (pairs)
  "(line . text) pairs as {line,text} JSON objects."
  (mapcar
   (lambda (pair)
     (json-object "line" (car pair) "text" (cdr pair)))
   pairs))
