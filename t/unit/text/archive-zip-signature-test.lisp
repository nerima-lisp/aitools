(in-package #:aitools.text.test)

(describe "aitools.text.domain zip central-directory signature"
  (it "rejects a corrupted central-directory entry signature"
    (let* ((archive
             (write-zip
              (list (make-archive-member :name "a" :data (string-bytes "x")))))
           (eocd (- (length archive) 22))
           (directory-offset
             (logior (aref archive (+ eocd 16))
                     (ash (aref archive (+ eocd 17)) 8))))
      (put-u32 archive directory-offset 0)
      (expect (zip-entries-reason archive)
              :to-equal
              (list (quote archive-error)
                    "bad central directory entry signature")))))
