;;;; packages/core/workspace/src/domain/git-index.lisp
;;;;
;;;; The path list of a git index file (index-format.txt, versions 2 to 4).
;;;; Ignore decisions compare against `git ls-files --cached --others
;;;; --exclude-standard`, and `--cached` lists tracked files even when an
;;;; ignore pattern matches them, so the scan needs the tracked set. Only
;;;; entry names are read; stat data, object ids, and extensions are
;;;; skipped. A split index (the `link` extension) is not followed.
(in-package #:aitools.workspace.domain)

(define-condition git-index-error (error)
  ((reason :initarg :reason :reader git-index-error-reason))
  (:report (lambda (condition stream)
             (format stream "malformed git index: ~A" (git-index-error-reason condition)))))

(defun %index-fail (reason)
  (error 'git-index-error :reason reason))

(defun %u32 (octets offset)
  (when (> (+ offset 4) (length octets)) (%index-fail "truncated"))
  (logior (ash (aref octets offset) 24) (ash (aref octets (+ offset 1)) 16)
          (ash (aref octets (+ offset 2)) 8) (aref octets (+ offset 3))))

(defun %u16 (octets offset)
  (when (> (+ offset 2) (length octets)) (%index-fail "truncated"))
  (logior (ash (aref octets offset) 8) (aref octets (+ offset 1))))

(defun %index-varint (octets offset)
  "git's offset varint (varint.c decode_varint). Returns (VALUES VALUE NEXT)."
  (let* ((limit (length octets))
         (byte (if (< offset limit) (aref octets offset) (%index-fail "truncated varint")))
         (value (logand byte 127)))
    (incf offset)
    (loop while (logbitp 7 byte)
          do (when (>= offset limit) (%index-fail "truncated varint"))
             (setf byte (aref octets offset)
                   value (+ (ash (1+ value) 7) (logand byte 127)))
             (incf offset)
             (when (> value most-positive-fixnum) (%index-fail "varint overflow")))
    (values value offset)))

(defun %index-name-octets (octets start)
  (let ((end (position 0 octets :start start)))
    (unless end (%index-fail "unterminated entry name"))
    end))

(defun parse-git-index-paths (octets &key (hash-size 20))
  "A sorted simple-vector of the distinct entry paths in OCTETS, a git index
file. HASH-SIZE is 20 for SHA-1 repositories and 32 for SHA-256 ones.
Signals GIT-INDEX-ERROR on malformed input."
  (unless (and (>= (length octets) 12)
               (= (aref octets 0) 68) (= (aref octets 1) 73)
               (= (aref octets 2) 82) (= (aref octets 3) 67))
    (%index-fail "missing DIRC signature"))
  (let* ((version (%u32 octets 4))
         (count (%u32 octets 8))
         (flags-offset (+ 40 hash-size))
         (offset 12)
         (previous (make-array 0 :element-type '(unsigned-byte 8)))
         (paths '()))
    (unless (member version '(2 3 4)) (%index-fail (format nil "unsupported version ~D" version)))
    (dotimes (i count)
      (let* ((flags (%u16 octets (+ offset flags-offset)))
             (extended (and (>= version 3) (logbitp 14 flags)))
             (name-start (+ offset flags-offset 2 (if extended 2 0))))
        (if (= version 4)
            (multiple-value-bind (strip suffix-start) (%index-varint octets name-start)
              (when (> strip (length previous)) (%index-fail "prefix strip exceeds previous name"))
              (let* ((suffix-end (%index-name-octets octets suffix-start))
                     (name (concatenate '(vector (unsigned-byte 8))
                                        (subseq previous 0 (- (length previous) strip))
                                        (subseq octets suffix-start suffix-end))))
                (setf previous name
                      offset (1+ suffix-end))))
            (let* ((name-end (%index-name-octets octets name-start))
                   (entry-length (- (+ name-end 1) offset)))
              (setf previous (subseq octets name-start name-end)
                    offset (+ offset (* 8 (ceiling entry-length 8))))))
        (push (cl-codec-kit:octets-to-string previous :encoding :utf-8 :errorp nil) paths)))
    (let ((sorted (sort paths #'string<)))
      (coerce (loop for (path . rest) on sorted
                    unless (and rest (string= path (first rest))) collect path)
              'simple-vector))))

(defun %lower-bound (paths key)
  (let ((low 0) (high (length paths)))
    (loop while (< low high)
          do (let ((mid (floor (+ low high) 2)))
               (if (string< (svref paths mid) key)
                   (setf low (1+ mid))
                   (setf high mid))))
    low))

(defun sorted-paths-contains-p (paths path)
  (let ((index (%lower-bound paths path)))
    (and (< index (length paths)) (string= (svref paths index) path))))

(defun sorted-paths-have-prefix-p (paths prefix)
  "True when some element of the sorted vector PATHS starts with PREFIX."
  (let ((index (%lower-bound paths prefix)))
    (and (< index (length paths))
         (let ((candidate (svref paths index)))
           (and (>= (length candidate) (length prefix))
                (string= prefix candidate :end2 (length prefix)))))))
