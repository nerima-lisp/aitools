(in-package #:aitools.e2e.test)

;;;; Correspondence table row 37: archive extraction and creation.

(define-row-case (37 "archive create's .gz is what gzip makes, read back by gzip -dc" :foreign ("gzip")) (ws)
  (put-archive-sources ws)
  (run-ok ws '("archive" "create" "a.gz" "src/a.txt"))
  (expect (utf8 (oracle ws '("gzip") "gzip -dc a.gz" nil :octets t))
          :to-equal (utf8 (oracle ws '("gzip") "gzip -c src/a.txt | gzip -dc" nil :octets t))))

(define-row-case (37 "archive create --format zip is what zip -r makes, read back by unzip" :foreign ("zip" "unzip")) (ws)
  (put-archive-sources ws)
  (run-ok ws '("archive" "create" "--format" "zip" "new.zip" "src"))
  (oracle ws '("zip" "unzip") "zip -qr ref.zip src && unzip -q ref.zip -d o && unzip -q new.zip -d a" nil)
  (expect (mapcar (lambda (entry) (list (first entry) (fourth entry))) (tree-snapshot ws "a/"))
          :to-equalp (mapcar (lambda (entry) (list (first entry) (fourth entry))) (tree-snapshot ws "o/"))))

(define-row-case (37 "archive create's .tar.gz is what tar -czf makes, read back by tar -xzf" :foreign ("tar")) (ws)
  (put-archive-sources ws)
  (run-ok ws '("archive" "create" "new.tar.gz" "src"))
  (oracle ws '("tar") "mkdir o && tar -czf ref.tgz src && tar -xzf ref.tgz -C o && mkdir a && tar -xzf new.tar.gz -C a" nil)
  (expect (mapcar #'fourth (tree-snapshot ws "a/")) :to-equalp (mapcar #'fourth (tree-snapshot ws "o/")))
  (expect (mapcar #'first (tree-snapshot ws "a/")) :to-equal (mapcar #'first (tree-snapshot ws "o/"))))

(define-row-case (37 "unzip is archive extract --to" :foreign ("unzip")) (ws)
  (make-archives ws)
  (oracle ws '("unzip") "unzip -q z.zip -d o" nil)
  (run-ok ws '("archive" "extract" "z.zip" "--to" "a"))
  (expect (mapcar (lambda (entry) (list (first entry) (second entry) (fourth entry))) (tree-snapshot ws "a/"))
          :to-equalp (mapcar (lambda (entry) (list (first entry) (second entry) (fourth entry))) (tree-snapshot ws "o/"))))

(define-row-case (37 "tar -xzf is archive extract --to" :foreign ("tar")) (ws)
  (make-archives ws)
  (oracle ws '("tar") "mkdir o && tar -xzf t.tgz -C o" nil)
  (run-ok ws '("archive" "extract" "t.tgz" "--to" "a"))
  (expect (mapcar #'butlast (tree-snapshot ws "a/")) :to-equalp (mapcar #'butlast (tree-snapshot ws "o/")))
  (expect (mapcar #'fourth (tree-snapshot ws "a/")) :to-equalp (mapcar #'fourth (tree-snapshot ws "o/"))))
