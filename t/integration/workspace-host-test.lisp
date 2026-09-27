;;;; t/integration/workspace-host-test.lisp
;;;;
;;;; The production WORKSPACE-HOST against a real filesystem: odd file
;;;; names, symlinks, FIFOs, the workspace boundary with real symlinks, the
;;;; CPU-sized ordered mapper, and the cl-boundary-kit registration.
(in-package #:aitools.workspace.integration-test)

(defun host-for (directory)
  (aitools.workspace.infrastructure:make-host-workspace-host
   :getenv (constantly nil)
   :home-directory (constantly directory)
   :current-directory (constantly directory)))

(defun root-for (host directory)
  (aitools.workspace.application:call-with-resolved-root/k
   host :root directory :on-resolved #'identity
        :on-error (lambda (reason path) (error "root ~A ~A" reason path))))

(defun entry-kinds (host directory)
  (sort (mapcar (lambda (entry)
                  (list (aitools.workspace.application:workspace-entry-name entry)
                        (aitools.workspace.application:workspace-entry-kind entry)))
                (aitools.workspace.application:host-list-directory host directory))
        #'string< :key #'first))

(describe "aitools workspace production host"
  (it "lists names with glob and escape characters and reports lstat kinds"
    (with-scratch-directory (scratch)
      (write-file (concatenate 'string scratch "/a*b") "1")
      (write-file (concatenate 'string scratch "/[x]?") "2")
      (write-file (concatenate 'string scratch "/back\\slash") "3")
      (write-file (concatenate 'string scratch "/sub/.hidden") "4")
      (make-symlink "sub" (concatenate 'string scratch "/link"))
      (let ((host (host-for scratch)))
        (expect (entry-kinds host scratch)
                :to-equal '(("[x]?" :file) ("a*b" :file) ("back\\slash" :file) ("link" :symlink) ("sub" :directory)))
        (expect (aitools.workspace.application:host-read-link host (concatenate 'string scratch "/link"))
                :to-equal "sub")
        (expect (map 'string #'code-char
                     (aitools.workspace.application:host-read-octets host (concatenate 'string scratch "/a*b")))
                :to-equal "1")
        (expect (aitools.workspace.application:host-stat host (concatenate 'string scratch "/missing"))
                :to-be-falsy))))

  (it "refuses to read a FIFO instead of blocking on it"
    (with-scratch-directory (scratch)
      (let ((fifo (concatenate 'string scratch "/pipe")))
        (sb-posix:mkfifo fifo #o600)
        (expect (aitools.workspace.infrastructure:read-regular-file-octets fifo) :to-be-falsy)
        (expect (aitools.workspace.infrastructure:read-regular-file-octets scratch) :to-be-falsy))))

  (it "scans a real tree in path order, skipping builtin excludes outside git"
    (with-scratch-directory (scratch)
      (dolist (file '("b.txt" "a/c.txt" "a.txt" "node_modules/x.js" "a*b.txt"))
        (write-file (concatenate 'string scratch "/" file) file))
      (let* ((host (host-for scratch)) (paths '()))
        (aitools.workspace.application:call-with-workspace-scan/k
         host (root-for host scratch)
         :work (lambda (entry) (aitools.workspace.application:scan-entry-size entry))
         :emit (lambda (entry size) (push (list (aitools.workspace.application:scan-entry-path entry) size) paths) nil)
         :on-complete (lambda (source stopped) (declare (ignore stopped)) (expect source :to-be :builtin))
         :on-error (lambda (reason path) (fail (format nil "~A ~A" reason path))))
        (expect (mapcar #'first (reverse paths)) :to-equal '("a*b.txt" "a.txt" "a" "a/c.txt" "b.txt"))
        (expect (second (assoc "a/c.txt" paths :test #'string=)) :to-be 7))))

  (it "enforces the workspace boundary with real symlinks and a real .git directory"
    (with-scratch-directory (scratch)
      (let ((root-path (concatenate 'string scratch "/ws")))
        (make-directories (concatenate 'string root-path "/.git"))
        (make-directories (concatenate 'string scratch "/outside"))
        (make-symlink "../outside" (concatenate 'string root-path "/escape"))
        (make-symlink "src" (concatenate 'string root-path "/alias"))
        (make-directories (concatenate 'string root-path "/src"))
        (let* ((host (host-for scratch))
               (root (root-for host root-path)))
          (flet ((verdict (target)
                   (aitools.workspace.application:call-with-workspace-boundary/k
                    host root target
                    :temporary-root (concatenate 'string scratch "/state/tmp")
                    :on-inside (lambda (path verdict) (declare (ignore path)) verdict)
                    :on-outside (lambda (verdict lexical real) (declare (ignore lexical real)) verdict))))
            (expect (verdict "src/new.lisp") :to-be :inside)
            (expect (verdict "alias/new.lisp") :to-be :inside)
            (expect (verdict "escape/new.lisp") :to-be :symlink-escape)
            (expect (verdict "../outside/x") :to-be :outside-root)
            (expect (verdict ".git/config") :to-be :git-directory)
            (expect (verdict (concatenate 'string scratch "/state/tmp/scratch.txt")) :to-be :temporary))))))

  (it "maps in input order on the CPU-sized pool"
    (expect (>= (aitools.workspace.infrastructure:processor-count) 1) :to-be-truthy)
    (let ((items (loop for i from 0 below 500 collect i)))
      (expect (aitools.workspace.infrastructure:call-with-ordered-mapper
               (lambda (mapper) (funcall mapper (lambda (x) (* x x)) items)))
              :to-equal (mapcar (lambda (x) (* x x)) items))))

  (it "registers the host in a cl-boundary-kit boundary context"
    (let* ((host (host-for "/"))
           (context (aitools.workspace.infrastructure:with-workspace-boundaries
                     (cl-boundary-kit:make-boundary-context) :host host)))
      (expect (aitools.workspace.infrastructure:workspace-host-from-context context) :to-be host))))

(describe "aitools workspace production host ports"
  (it "answers NIL for what each reader cannot read"
    (with-scratch-directory (scratch)
      (let ((file (concatenate 'string scratch "/plain.txt"))
            (missing (concatenate 'string scratch "/missing"))
            (host (host-for scratch)))
        (write-file file "p")
        (expect (aitools.workspace.application:host-read-link host file) :to-be nil)
        (expect (aitools.workspace.application:host-read-octets host missing) :to-be nil)
        (expect (multiple-value-list (aitools.workspace.application:host-list-directory host missing))
                :to-equal '(nil nil))
        (expect (multiple-value-list (aitools.workspace.application:host-list-directory host file))
                :to-equal '(nil nil)))))

  (it "reports a FIFO as :other and reads a file larger than one buffer whole"
    (with-scratch-directory (scratch)
      (let ((fifo (concatenate 'string scratch "/pipe"))
            (big (concatenate 'string scratch "/big.bin"))
            (host (host-for scratch)))
        (sb-posix:mkfifo fifo #o600)
        (with-open-file (out (sb-ext:parse-native-namestring big) :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (make-array 70000 :element-type '(unsigned-byte 8) :initial-element 7) out))
        (expect (aitools.workspace.application:workspace-entry-kind
                 (aitools.workspace.application:host-stat host fifo))
                :to-be :other)
        (let ((octets (aitools.workspace.application:host-read-octets host big)))
          (expect (length octets) :to-be 70000)
          (expect (every (lambda (octet) (= octet 7)) octets) :to-be t)))))

  (it "reports the root directory as / and any other directory without a trailing slash"
    ;; The non-root directory is a scratch directory, not a system one: the
    ;; Nix build sandbox has no /usr.
    (with-scratch-directory (scratch)
      (let ((previous (sb-posix:getcwd))
            (host (aitools.workspace.infrastructure:make-host-workspace-host :getenv (constantly nil))))
        (unwind-protect
             (progn
               (sb-posix:chdir "/")
               (expect (aitools.workspace.application:host-current-directory host) :to-equal "/")
               (sb-posix:chdir (concatenate 'string scratch "/"))
               (expect (aitools.workspace.application:host-current-directory host) :to-equal scratch))
          (sb-posix:chdir previous)))))

  (it "refuses a host port that is not a function"
    (expect (handler-case (aitools.workspace.application:make-workspace-host
                           :list-directory #'identity :stat 42 :read-link #'identity :read-octets #'identity
                           :getenv #'identity :home-directory #'identity :current-directory #'identity)
              (error (condition) (princ-to-string condition)))
            :to-equal "make-workspace-host: STAT must be a function, got 42")))

(describe "aitools workspace scan under a root reached through a symlink"
  (it "maps a start given by its real path into the root named through the link"
    ;; `--root /tmp/x` from the working directory /private/tmp/x on macOS:
    ;; the root is lexical, a start typed from the working directory is real.
    (with-scratch-directory (scratch)
      (let ((real-root (concatenate 'string scratch "/real/ws"))
            (linked-root (concatenate 'string scratch "/link/ws")))
        (write-file (concatenate 'string real-root "/a.txt") "a")
        (write-file (concatenate 'string real-root "/sub/b.txt") "b")
        (write-file (concatenate 'string scratch "/elsewhere/c.txt") "c")
        (make-symlink "real" (concatenate 'string scratch "/link"))
        (let* ((host (host-for scratch))
               (root (root-for host linked-root)))
          (flet ((scan (paths)
                   (let ((found '()))
                     (aitools.workspace.application:call-with-workspace-scan/k
                      host root
                      :paths paths
                      :emit (lambda (entry result)
                              (declare (ignore result))
                              (push (aitools.workspace.application:scan-entry-path entry) found)
                              nil)
                      :on-complete (lambda (source stopped)
                                     (declare (ignore source stopped))
                                     (sort found #'string<))
                      :on-error (lambda (reason path) (list reason path))))))
            (expect (aitools.workspace.application:workspace-root-path root) :to-equal linked-root)
            (expect (scan (list (concatenate 'string real-root "/a.txt") (concatenate 'string real-root "/sub")))
                    :to-equal '("a.txt" "sub/b.txt"))
            (expect (scan (list (concatenate 'string linked-root "/a.txt"))) :to-equal '("a.txt"))
            (expect (scan (list (concatenate 'string scratch "/elsewhere/c.txt")))
                    :to-equal (list :outside-root (concatenate 'string scratch "/elsewhere/c.txt")))
            (expect (aitools.workspace.application:workspace-path-ignored-p
                     host root (concatenate 'string real-root "/.git/config"))
                    :to-be-truthy)))))))
