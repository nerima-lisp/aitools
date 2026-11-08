(in-package #:cl-user)

(defun retired-document-section ()
  (with-output-to-string (out)
    (format out "| Retired command names | Current command |~%|---|---|~%")
    (dolist (row aitools.data:*correspondence-table*)
      (let ((names
             (remove-if-not (function retired-document-command-name-p)
                            (getf row :foreign-names))))
        (when names
          (let ((display-names (format nil "~{`~A`~^, ~}" names)))
            (dolist (repair (getf row :repairs))
              (format out
                      "| ~A | `~A` |~%"
                      display-names
                      (getf repair :command)))))))))

(defun correspondence-section ()
  (with-output-to-string (out)
    (format out
            "| Shell command | aitools command | What it does |~%|---|---|---|~%")
    (dolist (row aitools.data:*correspondence-table*)
      (let ((names
             (remove-if (function retired-document-command-name-p)
                        (getf row :foreign-names))))
        (when names
          (let ((display-names (format nil "~{`~A`~^, ~}" names)))
            (dolist (repair (getf row :repairs))
              (format out
                      "| ~A | `~A` | ~A |~%"
                      display-names
                      (getf repair :command)
                      (cell (getf repair :detail))))))))))

(defun retired-document-command-name-p (name)
  (let ((separator (position #\. name)))
    (and separator
         (member (subseq name 0 separator)
                 (list "json" "yaml" "toml")
                 :test
                 (function string=)))))

(defun cell (text)
  "TEXT made safe for one Markdown table cell."
  (with-output-to-string (out)
    (loop for char across (princ-to-string text)
          do (case char
               (#\| (write-string "\\|" out))
               (#\Newline (write-char #\Space out))
               (t (write-char char out))))))
