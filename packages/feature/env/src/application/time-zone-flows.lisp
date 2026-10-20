(in-package #:aitools.env.application)

(aitools.kernel.domain:define-flow
    (resolve-zone/k ports name &key on-zone on-unknown on-unavailable)
  "Call ON-ZONE with an IANA zone, ON-UNKNOWN when no matching TZif file
exists, or ON-UNAVAILABLE when no zoneinfo directory exists."
  (cond
    ((aitools.env.domain:utc-zone-name-p name)
     (aitools.kernel.domain:flow-continue
      :on-zone (aitools.env.domain:make-fixed-zone name 0)))
    ((not (aitools.env.domain:valid-zone-name-p name))
     (aitools.kernel.domain:flow-continue :on-unknown))
    (t
     (let ((any-directory nil)
           (zone nil))
       (dolist (directory (aitools.env.domain:zoneinfo-directories (%getenv ports "TZDIR")))
         (when (funcall (env-ports-list-directory ports) directory)
           (setf any-directory t)
           (let ((octets (%read-octets ports (aitools.env.domain:join-directory directory name))))
             (when octets
               (setf zone
                     (handler-case
                         (aitools.env.domain:parse-tzif name octets)
                       (aitools.env.domain:tzif-format-error () nil)))
               (when zone (return))))))
       (cond
         (zone
          (aitools.kernel.domain:flow-continue :on-zone zone))
         (any-directory
          (aitools.kernel.domain:flow-continue :on-unknown))
         (t
          (aitools.kernel.domain:flow-continue :on-unavailable)))))))

(aitools.kernel.domain:define-flow
    (%with-zone/k ports name &key on-zone on-error)
  (aitools.kernel.domain:flow-call
      (resolve-zone/k ports name)
    (:on-zone (zone)
      (aitools.kernel.domain:flow-continue :on-zone zone))
    (:on-unknown ()
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid"
       (format nil "unknown time zone ~S; use an IANA name such as Asia/Tokyo" name)
       :repairs (list (repair "use-utc"
                              "Use UTC, or an IANA zone name."
                              "aitools time now --tz UTC"))))
    (:on-unavailable ()
      (aitools.kernel.domain:flow-continue
       :on-error
       "environment.unavailable"
       "no zoneinfo database found; set TZDIR to its directory"
       :repairs (list (repair "use-utc"
                              "UTC needs no zoneinfo database."
                              "aitools time now --tz UTC"))))))

(defun %zone-iso8601 (zone instant)
  (aitools.env.domain:format-iso8601
   instant (aitools.env.domain:zone-offset-at zone instant)
   :utc-designator (aitools.env.domain:utc-zone-name-p (aitools.env.domain:zone-name zone))))
