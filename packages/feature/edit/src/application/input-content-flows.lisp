(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (read-input-file/k ports path &key on-octets on-error)
  "--content-file PATH (relative to the working directory, outside the
workspace allowed: reads are not bounded by the workspace boundary) as bytes."
  (let ((source (edit-ports-text-source ports))
        (absolute (%absolute-input-path ports path)))
    (multiple-value-bind (size problem) (aitools.text.application:source-file-size source absolute)
      (cond
        ((eq problem :unreadable)
         (aitools.kernel.domain:flow-continue
           :on-error
           "environment.io"
           (format
             nil
             "cannot read ~A"
             path)))
        ((null size)
         (aitools.kernel.domain:flow-continue
           :on-error
           "input.not-found"
           (format
             nil
             "--content-file ~A does not exist"
             path)))
        ((> size +max-input-bytes+)
         (aitools.kernel.domain:flow-continue
           :on-error
           "refusal.too-large"
           (format
             nil
             "--content-file ~A exceeds ~D bytes"
             path
             +max-input-bytes+)))
        (t
         (let ((octets (aitools.text.application:source-read-octets source absolute)))
           (if octets
               (aitools.kernel.domain:flow-continue
                 :on-octets
                 (coerce octets 'octets))
               (aitools.kernel.domain:flow-continue
                 :on-error
                 "environment.io"
                 (format
                   nil
                   "cannot read ~A"
                   path)))))))))

(aitools.kernel.domain:define-flow
  (read-content/k ports options &key on-content on-error)
  "The single content input: exactly one of --content, --content-file,
--stdin (or --stdin-data). ON-CONTENT (octets source) where SOURCE is
:CONTENT, :CONTENT-FILE or :STDIN."
  (let ((given
         (remove nil
                 (list (and (getf options :content) :content)
                       (and (getf options :content-file) :content-file)
                       (and (or (getf options :stdin) (getf options :stdin-data)) :stdin)))))
    (cond
      ((null given)
       (aitools.kernel.domain:flow-continue
         :on-error
         "argument.invalid"
         "no content: pass --content, --content-file, or --stdin (stdin is never read implicitly)"))
      ((rest given)
       (aitools.kernel.domain:flow-continue
         :on-error
         "argument.invalid"
         "pass only one of --content, --content-file, --stdin"))
      (t
       (ecase (first given)
         (:content
          (aitools.kernel.domain:flow-continue
            :on-content
            (aitools.text.domain:encode-utf8 (getf options :content))
            :content))
         (:content-file
          (aitools.kernel.domain:flow-call
            (read-input-file/k ports (getf options :content-file))
            (:on-octets (octets)
              (aitools.kernel.domain:flow-continue
                :on-content
                octets
                :content-file))
            (:on-error (code message)
              (aitools.kernel.domain:flow-continue
                :on-error
                code
                message))))
         (:stdin
          (aitools.kernel.domain:flow-call
            (read-stdin/k ports options)
            (:on-octets (octets)
              (aitools.kernel.domain:flow-continue
                :on-content
                octets
                :stdin))
            (:on-error (code message)
              (aitools.kernel.domain:flow-continue
                :on-error
                code
                message)))))))))

(aitools.kernel.domain:define-flow
  (read-content-text/k ports options &key on-text on-error)
  "READ-CONTENT/K decoded as UTF-8 text (text commands)."
  (aitools.kernel.domain:flow-call
    (read-content/k ports options)
    (:on-content (octets source)
      (aitools.kernel.domain:flow-call
        (%utf8-text/k octets)
        (:on-text (text)
          (aitools.kernel.domain:flow-continue
            :on-text
            text))
        (:on-invalid (offset)
          (aitools.kernel.domain:flow-continue
            :on-error
            "input.not-utf8"
            (format
              nil
              "the ~(~A~) input is not UTF-8 at byte ~D"
              source
              offset)))))
    (:on-error (code message)
      (aitools.kernel.domain:flow-continue
        :on-error
        code
        message))))

(defun inline-content-options (options text)
  "OPTIONS with the content input recorded as --content TEXT, so a replayed
op does not depend on a file or stdin that may be gone."
  (let ((copy (copy-list options)))
    (dolist (key '(:content-file :stdin :stdin-data))
      (remf copy key))
    (setf (getf copy :content) text)
    copy))
