;;;;
;;;; Case-side harness: per-case workspaces, the checked aitools invocation,
;;;; JSON helpers, shell oracles, and define-row-case registration.
;;;;
;;;; Expected values come from the shell command the table row replaces,
;;;; run through /bin/sh in a sibling copy of the same fixture. When a tool
;;;; the command needs is not on PATH the case takes a counted skip naming
;;;; the missing tool; when the tool is present its output is compared and,
;;;; where the case carries a fixed expected value, that constant is
;;;; cross-checked against the tool's own output so it stays honest.
(in-package #:aitools.e2e.test)

;;; Workspaces

(defstruct workspace root state)

(defun git-environment ()
  '(("GIT_CONFIG_GLOBAL" . "/dev/null")
    ("GIT_CONFIG_NOSYSTEM" . "1")
    ("GIT_AUTHOR_NAME" . "e2e") ("GIT_AUTHOR_EMAIL" . "e2e@example.invalid")
    ("GIT_COMMITTER_NAME" . "e2e") ("GIT_COMMITTER_EMAIL" . "e2e@example.invalid")))

(defun workspace-environment (workspace &optional extra)
  (environment-with
   (append extra
           `(("XDG_STATE_HOME" . ,(native (workspace-state workspace)))
             ("LC_ALL" . "C")
             ("AITOOLS_E2E_BINARY" . nil))
           (git-environment))))

(defun call-with-workspace (function &key git)
  "Call FUNCTION with a fresh WORKSPACE (a `git init` repository when GIT),
removing it and its state directory afterwards."
  (let* ((n (incf *invocation-counter*))
         (root (scratch-path (format nil "~A-ws-~D/" *run-token* n)))
         (state (scratch-path (format nil "state-~D/" n))))
    (ensure-directories-exist root)
    (ensure-directories-exist state)
    (let ((workspace (make-workspace :root (truename root) :state (truename state))))
      (unwind-protect
           (progn
             (when git
               (let ((result (run-process "git" '("init" "-q" "-b" "main" ".")
                                          :directory (workspace-root workspace)
                                          :environment (workspace-environment workspace))))
                 (unless (eql (result-exit-code result) 0)
                   (fail "~A" (format nil "git init failed: ~A" (utf8 (result-stderr result)))))))
             (funcall function workspace))
        (dolist (path (list root state))
          (uiop:delete-directory-tree path :validate (lambda (p) (search *run-token* (namestring p)))
                                           :if-does-not-exist :ignore))))))

(defun ws-path (workspace relative)
  (merge-pathnames relative (workspace-root workspace)))

(defun put (workspace relative content &key mode)
  "Write CONTENT (a string, written as UTF-8, or an octet vector) to RELATIVE."
  (let ((path (ws-path workspace relative)))
    (write-octets path (if (stringp content) (octets content) content))
    (when mode
      (sb-posix:chmod (native path) mode))
    path))

(defun file-octets (workspace relative)
  (read-octets (ws-path workspace relative)))

(defun file-text (workspace relative)
  (utf8 (file-octets workspace relative)))

(defun file-mode (workspace relative)
  (logand #o7777 (sb-posix:stat-mode (sb-posix:lstat (native (ws-path workspace relative))))))

;;; aitools

(defstruct (aitools-result (:conc-name aitools-))
  exit-code envelope stream stdout stderr argv)

(defun jget (value &rest keys)
  "Walk VALUE by string member names and integer indexes; NIL when absent."
  (reduce (lambda (current key)
            (etypecase key
              (string (and (hash-table-p current) (values (gethash key current))))
              (integer (and (vectorp current) (not (stringp current))
                            (< key (length current)) (aref current key)))))
          keys :initial-value value))

(defun ok (result)
  (let* ((envelope (aitools-envelope result))
         (output (if (eq (aitools-stream result) :stdout) (aitools-stdout result) (aitools-stderr result))))
    (unless (equal (jget envelope "status") "ok") (fail "~A" output))
    (expect envelope :to-match-envelope "status" "ok")
    (unless (eql (aitools-exit-code result) 0)
      (fail "~A"
            (format nil "aitools ~{~A~^ ~} exited ~A: ~A" (aitools-argv result)
                    (aitools-exit-code result) output)))
    envelope))

(defun run-ok (workspace arguments &rest options) (ok (apply #'aitools workspace arguments options)))

(defun jlist (value)
  (coerce value 'list))

(defun lines-text (lines)
  "LINES (a JSON array of strings) as newline-terminated text."
  (format nil "~{~A~%~}" (jlist lines)))

(defun text-lines (text)
  "TEXT split on newlines, without the empty string after a final newline."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (if (and lines (string= (car (last lines)) ""))
        (butlast lines)
        lines)))

(defun json= (a b)
  "Structural JSON equality: objects by member set, arrays in order, numbers
by value."
  (cond ((and (hash-table-p a) (hash-table-p b))
         (and (= (hash-table-count a) (hash-table-count b))
              (loop for key being the hash-keys of a using (hash-value value)
                    always (multiple-value-bind (other present) (gethash key b)
                             (and present (json= value other))))))
        ((and (vectorp a) (vectorp b) (not (stringp a)) (not (stringp b)))
         (and (= (length a) (length b)) (every #'json= a b)))
        ((and (numberp a) (numberp b)) (= a b))
        ((and (stringp a) (stringp b)) (string= a b))
        (t (eq a b))))
