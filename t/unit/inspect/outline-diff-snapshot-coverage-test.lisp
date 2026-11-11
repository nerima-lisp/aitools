(in-package #:aitools.inspect.test)

(describe "outline heading level edges"
  (it "accepts a heading made of hashes and rejects plain or overlong lines"
    (expect (aitools.inspect.domain::%heading-level "######") :to-be 6)
    (expect (aitools.inspect.domain::%heading-level "plain text") :to-be nil)
    (expect (aitools.inspect.domain::%heading-level "#######") :to-be nil))

  (it "requires positive start and end lines in a definition"
    (signals type-error
      (aitools.inspect.domain::make-definition (parse-integer "0") 1 "function" "f"))
    (signals type-error
      (aitools.inspect.domain::make-definition 1 (parse-integer "0") "function" "f"))))

(describe "diff default options"
  (it "compares lines with the default context and final-newline settings"
    (let ((hunks (compare-lines #("before" "old" "after")
                                #("before" "new" "after"))))
      (expect (mapcar #'aitools.kernel.domain:diff-line-op
                      (aitools.kernel.domain:diff-hunk-lines (first hunks)))
              :to-equal '(:context :delete :insert :context)))))

(defun decode-snapshot-test-entry (created entry)
  (decode-snapshot/k
   (format nil
           "{\"snapshot_id\":\"snap-20231114T221320Z-0a1b2c3d\",\"created\":~A,\"options\":{\"glob\":[],\"no_ignore\":false,\"lang\":null,\"skip_larger_than\":null,\"newer\":null},\"files\":[~A]}"
           created entry)
   "snap-20231114T221320Z-0a1b2c3d"
   :on-snapshot (constantly :ok)
   :on-invalid (constantly :invalid)))

(describe "snapshot decoder entry edges"
  (it "rejects non-array and wrong-sized file records"
    (expect (decode-snapshot-test-entry "\"t\"" "1") :to-be :invalid)
    (expect (decode-snapshot-test-entry "\"t\"" "[\"a\", 1, 2]") :to-be :invalid))

  (it "rejects a non-string creation time before decoding file entries"
    (expect (decode-snapshot-test-entry "1" "[\"a\", 1, 2, \"h\"]") :to-be :invalid))

  (it "rejects a negative size in a directly constructed file record"
    (signals type-error
      (make-snapshot-file "a" (1- (parse-integer "0")) 0 "h"))))
