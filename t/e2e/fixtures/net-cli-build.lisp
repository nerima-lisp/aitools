(require :asdf)

(let* ((fixture-directory
         (make-pathname :name nil :type nil :defaults *load-truename*))
       (source-root (truename (merge-pathnames "../../../" fixture-directory)))
       (fasl-directory (uiop:getenv "AITOOLS_E2E_NET_CLI_FASL")))
  (unless (and fasl-directory (plusp (length fasl-directory)))
    (error "AITOOLS_E2E_NET_CLI_FASL is required."))
  (asdf:initialize-output-translations
   (list :output-translations
         (list t
               (list (uiop:ensure-directory-pathname fasl-directory)
                     :**/ :*.*.*))
         :ignore-inherited-configuration))
  (asdf:initialize-source-registry
   (list :source-registry (list :directory source-root) :inherit-configuration))
  (asdf:load-system "aitools/cli"))

(defun net-e2e-http-send (method uri timeout)
  (declare (ignore timeout))
  (cond
    ((string= method "GET")
     (list :status 200
           :headers '(("Content-Type" . "text/plain"))
           :body (sb-ext:string-to-octets (format nil "fixture response~%")
                                          :external-format :utf-8)
           :url uri))
    ((string= method "HEAD")
     (list :status 200
           :headers '(("Content-Type" . "text/plain"))
           :body #()
           :url uri))
    (t
     (error "Unexpected net HTTP method ~S." method))))

(defun net-e2e-lookup (host)
  (let ((mode (uiop:getenv "AITOOLS_E2E_NET_LOOKUP_MODE")))
    (cond
      ((string= mode "stall")
       (unless (string= host "stall.fixture.invalid")
         (error "Unexpected stalled resolver host ~S." host))
       (let* ((marker (uiop:getenv "AITOOLS_E2E_NET_LOOKUP_MARKER"))
              (port (uiop:getenv "AITOOLS_E2E_NET_LOOKUP_PORT"))
              (socket
                (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
              (stream nil))
         (unless marker
           (error "AITOOLS_E2E_NET_LOOKUP_MARKER is required in stall mode."))
         (unless port
           (error "AITOOLS_E2E_NET_LOOKUP_PORT is required in stall mode."))
         (with-open-file (output marker :direction :output :if-exists :supersede)
           (write (get-universal-time) :stream output)
           (terpri output))
         (unwind-protect
              (progn
                (sb-bsd-sockets:socket-connect socket #(127 0 0 1) (parse-integer port))
                (setf stream
                      (sb-bsd-sockets:socket-make-stream
                       socket :input t :output nil :element-type '(unsigned-byte 8)
                       :buffering :none :auto-close nil))
                (read-byte stream nil nil))
           (when stream
             (close stream))
           (sb-bsd-sockets:socket-close socket))
         nil))
      ((string= mode "resolved")
       (unless (string= host "fixture.example")
         (error "Unexpected fixture resolver host ~S." host))
       (list (cons :inet #(192 0 2 7))
             (cons :inet6 #(32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 1))))
      (t
       (error "AITOOLS_E2E_NET_LOOKUP_MODE is required.")))))

(defun net-e2e-dependencies (&rest arguments)
  (declare (ignore arguments))
  (let* ((boundaries (cl-boundary-kit:make-boundary-context))
         (resolver
           (aitools.net.infrastructure:make-os-resolver
            :lookup-function #'net-e2e-lookup)))
    (setf boundaries
          (cl-boundary-kit:boundary-context-with
           boundaries :net-http-send #'net-e2e-http-send))
    (cl-boundary-kit:boundary-context-with
     boundaries :net-resolve resolver)))

(defun net-e2e-image-entry-point ()
  (let ((registration
          (assoc :net aitools/cli::*context-command-registrations*)))
    (unless registration
      (error "The net context registration is unavailable."))
    (setf (third registration) (function net-e2e-dependencies)))
  (setf aitools/cli::*application*
        (multiple-value-list (aitools/cli::build-app)))
  (aitools/cli::image-entry-point))

(defmethod asdf:output-files :around
    ((operation asdf:program-op)
     (system (eql (asdf:find-system "aitools/cli"))))
  (declare (ignore operation system))
  (let ((target (uiop:getenv "AITOOLS_E2E_NET_CLI_OUTPUT")))
    (unless (and target (plusp (length target)))
      (error "AITOOLS_E2E_NET_CLI_OUTPUT is required."))
    (values (list (pathname target)) t)))

(progn
  (setf (asdf/system:component-entry-point (asdf:find-system "aitools/cli"))
        "CL-USER::NET-E2E-IMAGE-ENTRY-POINT")
  (asdf:operate 'asdf:program-op "aitools/cli"))
