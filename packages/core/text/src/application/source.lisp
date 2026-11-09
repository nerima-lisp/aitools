;;;;
;;;; The TEXT-SOURCE port and the binary-before-read flow. The port works per file or
;;;; per chunk, never per line; line work happens in the
;;;; domain over the returned bytes. Paths are absolute native strings.
(in-package #:aitools.text.application)

(defstruct (text-source (:constructor %make-text-source) (:copier nil))
  (file-size nil :type function :read-only t)
  (read-prefix nil :type function :read-only t)
  (read-octets nil :type function :read-only t)
  (call-with-chunks nil :type function :read-only t)
  (read-sniffed nil :type function :read-only t)
  (read-range nil :type function :read-only t))

(defun %composed-read-sniffed (file-size read-prefix read-octets)
  "READ-SNIFFED from the other three port functions, for a source that has
no single-open reader: a size, a prefix, and then the whole file, in up to
three opens. Bounded reads limit the prefix and forward MAX-BYTES to
READ-OCTETS, which must return (VALUES NIL :TOO-LARGE) without retaining more
than MAX-BYTES bytes."
  (lambda (path count continue-p &key max-bytes)
    (multiple-value-bind (size problem) (funcall file-size path)
      (if (null size)
          (values nil problem)
          (let ((read-count (if max-bytes (min count (1+ max-bytes)) count)))
            (multiple-value-bind (prefix problem) (funcall read-prefix path read-count)
              (if (null prefix)
                  (values nil problem)
                  (let ((continue (funcall continue-p prefix size)))
                    (cond ((and continue max-bytes (> (length prefix) max-bytes))
                           (values nil :too-large))
                          ((or (not continue) (< (length prefix) read-count)) prefix)
                          (max-bytes (funcall read-octets path :max-bytes max-bytes))
                          (t (funcall read-octets path))))))))))
)

(defun %composed-read-range (file-size call-with-chunks)
  "READ-RANGE from FILE-SIZE and CALL-WITH-CHUNKS, for a source that cannot
seek: the chunks before START are read and dropped, so memory stays bounded
by the range even though the bytes before it are still read."
  (lambda (path start end)
    (multiple-value-bind (size problem) (funcall file-size path)
      (if (null size)
          (values nil problem)
          (let* ((end (min end size))
                 (start (min start end))
                 (buffer (make-array (- end start) :element-type '(unsigned-byte 8)))
                 (position 0))
            (if (or (= start end)
                    (funcall call-with-chunks path 65536
                             (lambda (chunk)
                               (let ((chunk-end (+ position (length chunk))))
                                 (when (> chunk-end start)
                                   (replace buffer chunk :start1 (max 0 (- position start))
                                                         :start2 (max 0 (- start position))
                                                         :end2 (- (min end chunk-end) position)))
                                 (setf position chunk-end)
                                 (when (>= position end) :stop)))))
                (values buffer size)
                (values nil nil)))))))

(defun make-text-source (&key file-size read-prefix read-octets call-with-chunks read-sniffed read-range)
  "Build a TEXT-SOURCE. Each argument is a function:

FILE-SIZE (path) -> byte size of the regular file PATH, or NIL when PATH is
  missing or not a regular file (after following symlinks).
READ-PREFIX (path count) -> at most COUNT leading bytes, or NIL.
READ-OCTETS (path &key max-bytes) -> every byte, or NIL. A bounded call must
  stop after at most MAX-BYTES plus one byte and return (VALUES NIL :TOO-LARGE)
  when the file exceeds the limit.
CALL-WITH-CHUNKS (path chunk-size function) -> calls FUNCTION with each
  successive chunk (a fresh octet vector) until FUNCTION returns :STOP;
  returns T, or NIL when PATH could not be opened.
READ-SNIFFED (path count continue-p &key max-bytes), optional -> opens PATH
  once, reads at most COUNT leading bytes, and calls CONTINUE-P (prefix size)
  with them and the byte size. Returns the prefix when CONTINUE-P returns
  false, else every byte of the file read on the same descriptor. A bounded
  call returns (VALUES NIL :TOO-LARGE) if the file grows past MAX-BYTES; its
  reader must stop after at most MAX-BYTES plus one byte. When not given, it is
  composed from FILE-SIZE, READ-PREFIX, and READ-OCTETS; READ-OCTETS must honor
  MAX-BYTES for that bounded fallback.
READ-RANGE (path start end), optional -> (VALUES octets size): the bytes
  [START, END) clamped to the file, read without the bytes before START,
  and the byte size of the whole file, from one open; NIL when PATH could
  not be read. When not given, it is composed from FILE-SIZE and
  CALL-WITH-CHUNKS.

Every octet vector is a simple (unsigned-byte 8) vector. A function that
returns NIL because PATH exists but may not be read (permission denied)
returns :UNREADABLE as its second value; a lone NIL means missing."
  (flet ((need (value name)
           (unless (functionp value)
             (error "make-text-source: ~A must be a function, got ~S" name value))
           value))
    (%make-text-source :file-size (need file-size "FILE-SIZE")
                       :read-prefix (need read-prefix "READ-PREFIX")
                       :read-octets (need read-octets "READ-OCTETS")
                       :call-with-chunks (need call-with-chunks "CALL-WITH-CHUNKS")
                    :read-sniffed (if read-sniffed
                                      (need read-sniffed "READ-SNIFFED")
                                      (%composed-read-sniffed file-size read-prefix read-octets))
                       :read-range (if read-range
                                       (need read-range "READ-RANGE")
                                       (%composed-read-range file-size call-with-chunks)))))

(defun source-file-size (source path)
  (funcall (text-source-file-size source) path))

(defun source-read-prefix (source path count)
  (funcall (text-source-read-prefix source) path count))

(defun source-read-octets (source path &key max-bytes)
  (if max-bytes
      (funcall (text-source-read-octets source) path :max-bytes max-bytes)
      (funcall (text-source-read-octets source) path)))

(defun source-call-with-chunks (source path chunk-size function)
  (funcall (text-source-call-with-chunks source) path chunk-size function))

(defun source-read-sniffed (source path count continue-p &key max-bytes)
  (if max-bytes
      (funcall (text-source-read-sniffed source) path count continue-p :max-bytes max-bytes)
      (funcall (text-source-read-sniffed source) path count continue-p)))

(defun source-read-range (source path start end)
  (funcall (text-source-read-range source) path start end))

(aitools.kernel.domain:define-flow
  (call-with-sniffed-octets/k
   source path &key max-bytes on-text on-binary on-missing on-unreadable
   (on-too-large (lambda (size) (funcall nil size))))
  "The binary-before-read decision over one open of PATH: the first
+BINARY-SNIFF-LENGTH+ bytes decide. With MAX-BYTES smaller than that prefix,
MAX-BYTES plus one bytes decide instead. Only a text file within MAX-BYTES is
read further, on the same descriptor. A bounded reader that detects growth
past MAX-BYTES returns :TOO-LARGE after reading at most MAX-BYTES plus one
byte; ON-TOO-LARGE receives MAX-BYTES plus one as the observed lower bound.
Calls exactly one continuation:

ON-TEXT (octets): every byte of a text file.
ON-BINARY (prefix size): the sniffed prefix and byte size; nothing past the
  prefix is read.
ON-MISSING (path): PATH is absent or not a regular file.
ON-UNREADABLE (path): PATH exists but may not be read (EACCES).
ON-TOO-LARGE (size): a text file larger than MAX-BYTES (only when MAX-BYTES
  is given); nothing past the bounded prefix is read for a stat-size rejection."
  (declare (type function on-text on-binary on-missing on-unreadable))
  (let ((outcome :text) (file-size 0))
    (multiple-value-bind (octets problem)
        (if max-bytes
            (source-read-sniffed source path +binary-sniff-length+
                                 (lambda (prefix size)
                                   (setf file-size size
                                         outcome (cond ((binary-octets-p prefix) :binary)
                                                       ((and max-bytes (> size max-bytes)) :too-large)
                                                       (t :text)))
                                   (eq outcome :text))
                                 :max-bytes max-bytes)
            (source-read-sniffed source path +binary-sniff-length+
                                 (lambda (prefix size)
                                   (setf file-size size
                                         outcome (if (binary-octets-p prefix) :binary :text))
                                   (eq outcome :text))))
      (cond
        ((eq problem :too-large)
         (aitools.kernel.domain:flow-continue :on-too-large
                                              (and max-bytes (1+ max-bytes))))
        ((null octets)
         (if (eq problem :unreadable)
             (aitools.kernel.domain:flow-continue :on-unreadable path)
             (aitools.kernel.domain:flow-continue :on-missing path)))
        ((eq outcome :binary)
         (aitools.kernel.domain:flow-continue :on-binary octets file-size))
        ((eq outcome :too-large)
         (aitools.kernel.domain:flow-continue :on-too-large file-size))
        (t
         (aitools.kernel.domain:flow-continue :on-text octets)))))
)

(aitools.kernel.domain:define-flow
  (call-with-text-file/k
   source path &key max-bytes on-text on-binary on-missing
   (on-unreadable (lambda (path) (funcall on-missing path)))
   (on-too-large (lambda (size) (funcall nil size))))
  "CALL-WITH-SNIFFED-OCTETS/K with the text's layout: ON-TEXT (octets
layout) gets the whole file and its TEXT-LAYOUT. ON-MISSING (path) is called
for an unreadable PATH when ON-UNREADABLE is not given."
  (declare (type function on-text on-binary on-missing))
  (aitools.kernel.domain:flow-call
    (call-with-sniffed-octets/k source path :max-bytes max-bytes)
    (:on-text (octets)
      (aitools.kernel.domain:flow-continue
        :on-text octets (detect-text-layout octets)))
    (:on-binary (octets size)
      (aitools.kernel.domain:flow-continue :on-binary octets size))
    (:on-missing (missing-path)
      (aitools.kernel.domain:flow-continue :on-missing missing-path))
    (:on-unreadable (unreadable-path)
      (aitools.kernel.domain:flow-continue :on-unreadable unreadable-path))
    (:on-too-large (size)
      (aitools.kernel.domain:flow-continue :on-too-large size))))
