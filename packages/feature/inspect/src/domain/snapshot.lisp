;;;; packages/feature/inspect/src/domain/snapshot.lisp
;;;;
;;;; `snapshot create`/`snapshot diff` records: which files existed, with size, mtime,
;;;; and content hash, plus the scan options that chose them so `snapshot
;;;; diff` walks the same set. A record is a file in the user's state
;;;; directory and so is read back as untrusted input: DECODE-SNAPSHOT
;;;; rejects any shape it did not write.
(in-package #:aitools.inspect.domain)

(defstruct (snapshot-file (:constructor make-snapshot-file (path size mtime hash)) (:copier nil))
  (path "" :type string :read-only t)
  (size 0 :type (integer 0) :read-only t)
  (mtime 0 :type integer :read-only t)
  (hash "" :type string :read-only t))

(defstruct (snapshot (:constructor make-snapshot (id created files &key glob lang no-ignore skip-larger-than newer))
                     (:copier nil))
  "FILES is a list of SNAPSHOT-FILEs sorted by path. The option slots are
the `snapshot create` scan options (NEWER in Unix seconds)."
  (id "" :type string :read-only t)
  (created "" :type string :read-only t)
  (files '() :type list :read-only t)
  (glob '() :type list :read-only t)
  (lang nil :read-only t)
  (no-ignore nil :read-only t)
  (skip-larger-than nil :read-only t)
  (newer nil :read-only t))

(defun %hex-digit-p (char)
  (or (char<= #\0 char #\9) (char<= #\a char #\f)))

(defun valid-snapshot-id-p (id)
  "True for `snap-<YYYYMMDDTHHMMSSZ>-<8 lowercase hex>`, the only names
SNAPSHOT-ID-FROM-TIME makes; anything else (a path, `..`) is not an id."
  (and (stringp id)
       (= (length id) 30)
       (string= "snap-" id :end2 5)
       (every (lambda (char) (char<= #\0 char #\9)) (subseq id 5 13))
       (char= (char id 13) #\T)
       (every (lambda (char) (char<= #\0 char #\9)) (subseq id 14 20))
       (char= (char id 20) #\Z)
       (char= (char id 21) #\-)
       (every #'%hex-digit-p (subseq id 22))))

(defun snapshot-id-from-time (unix-seconds random-hex)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (aitools.kernel.domain:unix-seconds-to-universal-time unix-seconds) 0)
    (format nil "snap-~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ-~A" year month day hour minute second random-hex)))

(defun encode-snapshot (snapshot)
  (render-json
   (json-object "snapshot_id" (snapshot-id snapshot)
                "created" (snapshot-created snapshot)
                "options" (json-object "glob" (coerce (snapshot-glob snapshot) 'vector)
                                       "lang" (json-or-null (snapshot-lang snapshot))
                                       "no_ignore" (json-bool (snapshot-no-ignore snapshot))
                                       "skip_larger_than" (json-or-null (snapshot-skip-larger-than snapshot))
                                       "newer" (json-or-null (snapshot-newer snapshot)))
                "files" (map 'vector (lambda (file)
                                       (vector (snapshot-file-path file) (snapshot-file-size file)
                                               (snapshot-file-mtime file) (snapshot-file-hash file)))
                             (snapshot-files snapshot)))))

(defun %decoded-file (value)
  (unless (and (json-array-value-p value) (= (length value) 4)
               (stringp (aref value 0)) (typep (aref value 1) '(integer 0))
               (integerp (aref value 2)) (stringp (aref value 3)))
    (error "bad snapshot file entry"))
  (make-snapshot-file (aref value 0) (aref value 1) (aref value 2) (aref value 3)))

(defun %optional (value type)
  (cond ((json-null-value-p value) nil)
        ((typep value type) value)
        (t (error "bad snapshot option"))))

(defun %decode-snapshot-value (value id)
  (unless (json-object-value-p value) (error "snapshot is not an object"))
  (let ((options (json-object-get value "options"))
        (files (json-object-get value "files")))
    (unless (and (equal (json-object-get value "snapshot_id") id)
                 (stringp (json-object-get value "created"))
                 (json-object-value-p options) (json-array-value-p files))
      (error "bad snapshot header"))
    (let ((glob (json-object-get options "glob"))
          (no-ignore (json-object-get options "no_ignore")))
      (unless (and (json-array-value-p glob) (every #'stringp glob)
                   (or (eq no-ignore t) (json-false-value-p no-ignore)))
        (error "bad snapshot options"))
      (make-snapshot id (json-object-get value "created") (map 'list #'%decoded-file files)
                     :glob (coerce glob 'list)
                     :lang (%optional (json-object-get options "lang") 'string)
                     :no-ignore (eq no-ignore t)
                     :skip-larger-than (%optional (json-object-get options "skip_larger_than") '(integer 0))
                     :newer (%optional (json-object-get options "newer") 'integer)))))

(defun decode-snapshot/k (text id &key on-snapshot on-invalid)
  "Parse the record TEXT written for ID: ON-SNAPSHOT (snapshot) or
ON-INVALID () for anything ENCODE-SNAPSHOT would not have produced."
  (declare (type function on-snapshot on-invalid))
  (parse-json-document/k
   text
   :on-error (lambda (message line column) (declare (ignore message line column)) (funcall on-invalid))
   :on-value (lambda (value)
               (let ((snapshot (handler-case (%decode-snapshot-value value id)
                                 (error () nil))))
                 (if snapshot (funcall on-snapshot snapshot) (funcall on-invalid))))))

(defun compare-snapshot (snapshot current)
  "Compare SNAPSHOT with CURRENT, a list of SNAPSHOT-FILEs whose HASH may be
empty (not yet read). Returns (VALUES added removed suspects): path lists
sorted, and SUSPECTS the (recorded . current) pairs whose size or mtime
changed, to be settled by content hash."
  (let ((recorded (make-hash-table :test 'equal))
        (present (make-hash-table :test 'equal))
        (added '()) (suspects '()))
    (dolist (file (snapshot-files snapshot)) (setf (gethash (snapshot-file-path file) recorded) file))
    (dolist (file current)
      (setf (gethash (snapshot-file-path file) present) t)
      (let ((old (gethash (snapshot-file-path file) recorded)))
        (cond ((null old) (push (snapshot-file-path file) added))
              ((or (/= (snapshot-file-size old) (snapshot-file-size file))
                   (/= (snapshot-file-mtime old) (snapshot-file-mtime file)))
               (push (cons old file) suspects)))))
    (values (sort added #'string<)
            (sort (loop for file in (snapshot-files snapshot)
                        unless (gethash (snapshot-file-path file) present)
                          collect (snapshot-file-path file))
                  #'string<)
            (sort suspects #'string< :key (lambda (pair) (snapshot-file-path (car pair)))))))
