(in-package #:aitools.edit.application)

(defun %absolute-input-path (ports path)
  (aitools.workspace.application:user-path-absolute (edit-ports-workspace-host ports) path))

(defun read-input-file/k (ports path &key on-octets on-error)
  "--content-file PATH (relative to the working directory, outside the
workspace allowed: reads are not bounded by the workspace boundary) as bytes."
  (declare (type function on-octets on-error))
  (let ((source (edit-ports-text-source ports))
        (absolute (%absolute-input-path ports path)))
    (multiple-value-bind (size problem) (aitools.text.application:source-file-size source absolute)
      (cond
       ((eq problem :unreadable) (funcall on-error "environment.io" (format nil "cannot read ~A" path)))
       ((null size) (funcall on-error "input.not-found" (format nil "--content-file ~A does not exist" path)))
       ((> size +max-input-bytes+)
        (funcall on-error "refusal.too-large" (format nil "--content-file ~A exceeds ~D bytes" path
                                                      +max-input-bytes+)))
       (t (let ((octets (aitools.text.application:source-read-octets source absolute)))
            (if octets
                (funcall on-octets (coerce octets 'octets))
              (funcall on-error "environment.io" (format nil "cannot read ~A" path)))))))))

(defun read-content/k (ports options &key on-content on-error)
  "The single content input: exactly one of --content, --content-file,
--stdin (or --stdin-data). ON-CONTENT (octets source) where SOURCE is
:CONTENT, :CONTENT-FILE or :STDIN."
  (declare (type function on-content on-error))
  (let ((given (remove nil (list (and (getf options :content) :content)
                                 (and (getf options :content-file) :content-file)
                                 (and (or (getf options :stdin) (getf options :stdin-data)) :stdin)))))
    (cond
     ((null given)
      (funcall on-error "argument.invalid"
               "no content: pass --content, --content-file, or --stdin (stdin is never read implicitly)"))
     ((rest given)
      (funcall on-error "argument.invalid" "pass only one of --content, --content-file, --stdin"))
     (t (ecase (first given)
          (:content (funcall on-content (aitools.text.domain:encode-utf8 (getf options :content)) :content))
          (:content-file (read-input-file/k ports (getf options :content-file)
                                            :on-octets (lambda (octets) (funcall on-content octets :content-file))
                                            :on-error on-error))
          (:stdin (read-stdin/k ports options
                                :on-octets (lambda (octets) (funcall on-content octets :stdin))
                                :on-error on-error)))))))

(defun read-content-text/k (ports options &key on-text on-error)
  "READ-CONTENT/K decoded as UTF-8 text (text commands); ON-TEXT (text)."
  (declare (type function on-text on-error))
  (read-content/k ports options
                  :on-content (lambda (octets source)
                                (%utf8-text/k octets :on-text on-text
                                              :on-invalid (lambda (offset)
                                                            (funcall on-error "input.not-utf8"
                                                                     (format nil
                                                                             "the ~(~A~) input is not UTF-8 at byte ~D"
                                                                             source offset)))))
                  :on-error on-error))
