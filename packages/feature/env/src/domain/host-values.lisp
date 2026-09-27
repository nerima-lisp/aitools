;;;; packages/feature/env/src/domain/host-values.lisp
;;;;
;;;; Small value rules shared by the `sys` and `time` flows: which variable
;;;; names are secret, PATH search order, tool version lines, time zone
;;;; names and where their TZif files live, and the JSON null the flows put
;;;; in `path:null` and friends.
(in-package #:aitools.env.domain)

(defun json-null ()
  "The JSON null value (json-kit's sentinel). The application layer cannot
name json-kit directly, so it asks for it here."
  json-kit:+json-null+)

(defun secret-environment-name-p (name)
  "True when NAME contains one of the secret key names as a whole word.
Delegates to the protocol layer's canonical check so `sys env` redaction and
`key=value` masking share one word-boundary rule."
  (aitools.protocol.domain:secret-key-name-p name))

(defun split-search-path (path-value)
  "The directories of a PATH value, in order, skipping empty entries (an
empty entry would mean the current directory, which `sys tools` does not
search)."
  (let (directories (start 0))
    (loop for colon = (position #\: path-value :start start)
          do (let ((entry (subseq path-value start colon)))
               (when (plusp (length entry)) (push entry directories)))
             (if colon (setf start (1+ colon)) (return)))
    (remove-duplicates (nreverse directories) :test #'string= :from-end t)))

(defun join-directory (directory name)
  (if (and (plusp (length directory)) (char= (char directory (1- (length directory))) #\/))
      (concatenate 'string directory name)
      (concatenate 'string directory "/" name)))

(defun first-output-line (text)
  "The first nonblank line of TEXT, trimmed, or NIL."
  (loop for line in (%split-lines text)
        for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
        when (plusp (length trimmed)) return trimmed))

(defun valid-tool-name-p (name)
  "A bare command name: nonempty, no `/`, no NUL."
  (and (plusp (length name)) (not (find #\/ name)) (not (find (code-char 0) name))))

(defun valid-zone-name-p (name)
  "True for a relative IANA zone name such as `America/New_York`: ASCII
letters, digits, and `_+-.` in `/`-separated parts, none of them `.` or `..`,
so it can be joined under a zoneinfo directory without escaping it."
  (and (<= 1 (length name) 128)
       (every (lambda (char)
                (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9) (find char "_+-./")))
              name)
       (let ((parts (let (result (start 0))
                      (loop for slash = (position #\/ name :start start)
                            do (push (subseq name start slash) result)
                               (if slash (setf start (1+ slash)) (return)))
                      result)))
         (every (lambda (part) (and (plusp (length part)) (string/= part ".") (string/= part "..")))
                parts))))

(defparameter *utc-zone-names* '("UTC" "Etc/UTC" "Z" "Zulu" "Etc/Zulu" "UCT" "Etc/UCT")
  "Names answered without a TZif file, so UTC works even where no zoneinfo
database is installed.")

(defun utc-zone-name-p (name)
  (and (member name *utc-zone-names* :test #'string=) t))

(defun zoneinfo-directories (tzdir)
  "Directories searched for TZif files: $TZDIR first when set, then the
usual system locations (NixOS has only /etc/zoneinfo)."
  (append (when (and tzdir (plusp (length tzdir))) (list tzdir))
          '("/usr/share/zoneinfo" "/usr/lib/zoneinfo" "/usr/share/lib/zoneinfo" "/etc/zoneinfo")))

(defun zone-name-from-tz-variable (value)
  "The IANA name in a TZ variable (`Asia/Tokyo` or `:Asia/Tokyo`), or NIL
when VALUE is empty or a POSIX rule string such as `EST5EDT` that names no
file."
  (when (and value (plusp (length value)))
    (let ((name (string-left-trim ":" value)))
      (cond ((utc-zone-name-p name) name)
            ((and (valid-zone-name-p name) (find #\/ name)) name)))))

(defun zone-name-from-localtime-link (target)
  "The IANA name from the /etc/localtime symlink TARGET, i.e. the part after
the last `zoneinfo/` (`/var/db/timezone/zoneinfo/Asia/Tokyo` -> `Asia/Tokyo`)."
  (let ((marker (and target (search "zoneinfo/" target :from-end t))))
    (when marker
      (let ((name (subseq target (+ marker 9))))
        (and (valid-zone-name-p name) name)))))


(defun octets-from-latin-1 (text)
  "TEXT read with the latin-1 external format, back as the file's octets."
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8))))
    (dotimes (index (length text) octets)
      (setf (aref octets index) (char-code (char text index))))))
