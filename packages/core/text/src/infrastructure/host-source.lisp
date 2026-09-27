;;;; packages/core/text/src/infrastructure/host-source.lisp
;;;;
;;;; The production TEXT-SOURCE: reads of regular files. Each read opens with
;;;; O_NONBLOCK and checks the type with fstat on the open descriptor, so a
;;;; FIFO or device named by an agent is refused instead of blocking the
;;;; process, and the checked file is the one read (no stat-then-open window).
;;;; Paths stay native strings (no CL pathname parsing), so names with `*`,
;;;; `?`, `[`, or `\` work. Bytes come out of read(2) directly into a buffer
;;;; sized from fstat, with no intervening fd-stream and its per-file input
;;;; buffer, so a whole-file read allocates about one byte of heap per input
;;;; byte instead of an order of magnitude more.
(in-package #:aitools.text.infrastructure)

(defconstant +read-buffer-size+ 65536)

(defun %posix-read (fd sap count)
  "read(2) of at most COUNT bytes into SAP, retrying an interrupted call so a
signal delivered to a scan worker does not surface as a short read. Other
syscall errors propagate."
  (loop
    (handler-case (return (sb-posix:read fd sap count))
      (sb-posix:syscall-error (condition)
        (unless (= (sb-posix:syscall-errno condition) sb-posix:eintr)
          (error condition))))))

(defun %read-fully (fd buffer start count)
  "Read up to COUNT bytes into BUFFER at START through read(2), looping over
short reads until COUNT bytes are in hand or end of file is reached. Return
the number of bytes read; a result below COUNT means end of file, since each
short read below that was already followed up to the next zero-length read."
  (let ((got 0))
    (sb-sys:with-pinned-objects (buffer)
      (loop while (< got count)
            for n = (%posix-read fd
                                 (sb-sys:sap+ (sb-sys:vector-sap buffer) (+ start got))
                                 (- count got))
            while (plusp n)
            do (incf got n)))
    got))

(defun %shrink (buffer count)
  "BUFFER when it already holds exactly COUNT bytes, else its COUNT-byte prefix."
  (if (= count (length buffer)) buffer (subseq buffer 0 count)))

(defun %call-with-regular-file (path function)
  "Call FUNCTION with an open descriptor and the fstat size when PATH is (or
links to) a regular file and return its values. Otherwise return NIL without
calling it, with a second value :UNREADABLE when PATH exists but could not be
opened or examined (EACCES and the like): only ENOENT and ENOTDIR mean the
file is not there. A NUL names no file: open(2) would stop the name there and
read another file."
  (when (find (code-char 0) path)
    (return-from %call-with-regular-file nil))
  (let ((fd (handler-case (sb-posix:open path (logior sb-posix:o-rdonly sb-posix:o-nonblock))
              (sb-posix:syscall-error (condition)
                (return-from %call-with-regular-file
                  (if (member (sb-posix:syscall-errno condition) (list sb-posix:enoent sb-posix:enotdir))
                      nil
                      (values nil :unreadable))))
              ;; A name the file system encoding cannot represent names no file.
              (error () (return-from %call-with-regular-file nil)))))
    (unwind-protect
         (let ((stat (handler-case (sb-posix:fstat fd)
                       (sb-posix:syscall-error () (return-from %call-with-regular-file (values nil :unreadable))))))
           (when (sb-posix:s-isreg (sb-posix:stat-mode stat))
             (funcall function fd (sb-posix:stat-size stat))))
      (sb-posix:close fd))))

(defun %read-grown (head fd first-byte)
  "Concatenate HEAD (the fstat-sized prefix, all valid), FIRST-BYTE (the one
extra byte already read that proved the file grew past its stat size), and
the rest of FD read in chunks. Rare: a file appended to between fstat and
read. Chunked, never one byte at a time."
  (let ((chunks (list head)) (total (length head)))
    (let ((probe (make-array 1 :element-type '(unsigned-byte 8))))
      (setf (aref probe 0) first-byte)
      (push probe chunks)
      (incf total 1))
    (loop
      (let* ((chunk (make-array +read-buffer-size+ :element-type '(unsigned-byte 8)))
             (n (%read-fully fd chunk 0 +read-buffer-size+)))
        (when (plusp n)
          (push (%shrink chunk n) chunks)
          (incf total n))
        (when (< n +read-buffer-size+) (return))))
    (let ((result (make-array total :element-type '(unsigned-byte 8))) (offset 0))
      (dolist (chunk (nreverse chunks) result)
        (replace result chunk :start1 offset)
        (incf offset (length chunk))))))

(defun %read-all (fd size &optional head)
  "Every byte of the regular file behind FD, whose fstat size is SIZE, after
HEAD, the bytes already read from its start (none by default). One buffer of
SIZE is filled by read(2); a single one-byte probe past it names end of file
(the common case: the buffer is returned with no copy) or catches a file
grown since fstat and hands it to the chunked fallback. A file that shrank
returns its shorter prefix."
  (let* ((start (if head (length head) 0))
         (buffer (make-array (max size start) :element-type '(unsigned-byte 8)))
         (got (+ start (%read-fully fd buffer start (- (length buffer) start)))))
    (when head (replace buffer head))
    (if (< got size)
        (%shrink buffer got)
        (let* ((probe (make-array 1 :element-type '(unsigned-byte 8)))
               (extra (%read-fully fd probe 0 1)))
          (if (zerop extra)
              buffer
              (%read-grown buffer fd (aref probe 0)))))))

(defun %file-size (path)
  (%call-with-regular-file path (lambda (fd size) (declare (ignore fd)) size)))

(defun %read-prefix (path count)
  (%call-with-regular-file path
                           (lambda (fd size)
                             (declare (ignore size))
                             (let* ((buffer (make-array count :element-type '(unsigned-byte 8)))
                                    (got (%read-fully fd buffer 0 count)))
                               (%shrink buffer got)))))

(defun %read-sniffed (path count continue-p)
  (%call-with-regular-file path
                           (lambda (fd size)
                             (let* ((buffer (make-array count :element-type '(unsigned-byte 8)))
                                    (prefix (%shrink buffer (%read-fully fd buffer 0 count))))
                               (if (and (funcall continue-p prefix size) (= (length prefix) count))
                                   (%read-all fd size prefix)
                                   prefix)))))

(defun %read-octets (path)
  (%call-with-regular-file path (lambda (fd size) (%read-all fd size))))

(defun %read-range (path start end)
  "(VALUES octets size): the bytes [START, END) of PATH clamped to its fstat
size, read after one lseek so nothing before START is read."
  (%call-with-regular-file path
                           (lambda (fd size)
                             (let* ((end (min end size))
                                    (start (min start end))
                                    (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
                               (when (plusp start)
                                 (sb-posix:lseek fd start sb-posix:seek-set))
                               (values (%shrink buffer (%read-fully fd buffer 0 (length buffer))) size)))))

(defun %call-with-chunks (path chunk-size function)
  (%call-with-regular-file path
                           (lambda (fd size)
                             (declare (ignore size))
                             (loop
                               (let* ((buffer (make-array chunk-size :element-type '(unsigned-byte 8)))
                                      (n (%read-fully fd buffer 0 chunk-size)))
                                 (when (zerop n) (return))
                                 (when (eq (funcall function (%shrink buffer n)) :stop) (return))
                                 (when (< n chunk-size) (return))))
                             t)))

(defun make-host-text-source ()
  "The production TEXT-SOURCE over the real filesystem."
  (make-text-source :file-size #'%file-size
                    :read-prefix #'%read-prefix
                    :read-octets #'%read-octets
                    :call-with-chunks #'%call-with-chunks
                    :read-sniffed #'%read-sniffed
                    :read-range #'%read-range))
