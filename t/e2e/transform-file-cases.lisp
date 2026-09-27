;;;; t/e2e/transform-file-cases.lisp
;;;;
;;;; Correspondence table rows 22-30: whole-file transforms, normalization, encodings,
;;;; split, and file operations.
(in-package #:aitools.e2e.test)

;;; Row 22

(define-row-case (22 "LC_ALL=C sort is transform --op sort" :foreign ("sort")) (ws)
  (expect-same-edit ws (format nil "b~%B~%a~%10~%9~%a b~%") '("sort")
                    "sort o.txt" (format nil "10~%9~%B~%a~%a b~%b~%")
                    '("transform" "a.txt" "--op" "sort")))

(define-row-case (22 "sort -n is transform --op sort-numeric" :foreign ("sort")) (ws)
  (expect-same-edit ws (format nil "10~%9~%-1~%2.5~%0~%") '("sort")
                    "sort -n o.txt" (format nil "-1~%0~%2.5~%9~%10~%")
                    '("transform" "a.txt" "--op" "sort-numeric")))

(define-row-case (22 "sort -k2 is transform --op sort --key 2" :foreign ("sort")) (ws)
  (expect-same-edit ws (format nil "x 3~%y 1~%z 2~%") '("sort")
                    "sort -k2 o.txt" (format nil "y 1~%z 2~%x 3~%")
                    '("transform" "a.txt" "--op" "sort" "--key" "2")))

(define-row-case (22 "sort -V is transform --op sort-version" :foreign ("sort")) (ws)
  (expect-same-edit ws (format nil "1.10~%1.9~%1.2~%1.2.1~%") '("sort")
                    "sort -V o.txt" (format nil "1.2~%1.2.1~%1.9~%1.10~%")
                    '("transform" "a.txt" "--op" "sort-version")
                    :probe "echo 1 | sort -V"))

(define-row-case (22 "sort -u is transform --op sort --op unique" :foreign ("sort")) (ws)
  (expect-same-edit ws (format nil "b~%a~%b~%c~%a~%") '("sort")
                    "sort -u o.txt" (format nil "a~%b~%c~%")
                    '("transform" "a.txt" "--op" "sort" "--op" "unique")))

(define-row-case (22 "uniq on runs of duplicates is transform --op unique" :foreign ("uniq")) (ws)
  (expect-same-edit ws (format nil "a~%a~%b~%c~%c~%c~%") '("uniq")
                    "uniq o.txt" (format nil "a~%b~%c~%")
                    '("transform" "a.txt" "--op" "unique")))

(define-row-case (22 "tac is transform --op reverse" :foreign ("tac")) (ws)
  (expect-same-edit ws (format nil "a~%b~%c~%") '("tac")
                    "tac o.txt" (format nil "c~%b~%a~%")
                    '("transform" "a.txt" "--op" "reverse")))

(define-row-case (22 "shuf's permutation is transform --op shuffle, fixed by --seed" :foreign ("shuf")) (ws)
  (let ((content (format nil "a~%b~%c~%d~%e~%f~%")))
    (put ws "a.txt" content)
    (put ws "b.txt" content)
    (run-ok ws '("transform" "a.txt" "--op" "shuffle" "--seed" "7"))
    (run-ok ws '("transform" "b.txt" "--op" "shuffle" "--seed" "7"))
    (expect (file-text ws "a.txt") :to-equal (file-text ws "b.txt"))
    (expect (format nil "~{~A~%~}" (sorted (text-lines (file-text ws "a.txt"))))
            :to-equal (oracle ws '("sort") "sort b.txt" content))))

;;; Row 23

(define-row-case (23 "tr a-z A-Z is transform --op upper" :foreign ("tr")) (ws)
  (expect-same-edit ws (format nil "ab Cd~%xyz~%") '("tr")
                    "tr a-z A-Z < o.txt" (format nil "AB CD~%XYZ~%")
                    '("transform" "a.txt" "--op" "upper")))

(define-row-case (23 "dos2unix is transform --op eol-lf" :foreign ("dos2unix")) (ws)
  (expect-same-edit ws (format nil "a~C~%b~C~%" #\Return #\Return) '("dos2unix")
                    "dos2unix -q o.txt && cat o.txt" (format nil "a~%b~%")
                    '("transform" "a.txt" "--op" "eol-lf")))

(define-row-case (23 "expand is transform --op tabs-to-spaces" :foreign ("expand")) (ws)
  (expect-same-edit ws (format nil "a~Cb~%~Cc~%" #\Tab #\Tab) '("expand")
                    "expand o.txt" (format nil "a       b~%        c~%")
                    '("transform" "a.txt" "--op" "tabs-to-spaces")))

(define-row-case (23 "unexpand is transform --op spaces-to-tabs" :foreign ("unexpand")) (ws)
  (expect-same-edit ws (format nil "        a  b~%    c~%") '("unexpand")
                    "unexpand o.txt" (format nil "~Ca  b~%    c~%" #\Tab)
                    '("transform" "a.txt" "--op" "spaces-to-tabs")))

(define-row-case (23 "fold -s -w 12, without the blank fold keeps at each break, is transform --op wrap" :foreign ("fold")) (ws)
  (expect-same-edit ws (format nil "the quick brown fox jumps over the lazy dog~%short~%") '("fold" "sed")
                    "fold -s -w 12 o.txt | sed 's/ *$//'"
                    (format nil "the quick~%brown fox~%jumps over~%the lazy dog~%short~%")
                    '("transform" "a.txt" "--op" "wrap" "--columns" "12")))

;; GNU fmt aims for a goal width near 93% of -w and BSD fmt fills to the max,
;; so the same paragraph often wraps differently between them. This fixture is
;; chosen so both wrap it identically at -w 15 (`one two three` / `four`, blank
;; kept, `five six seven` on one line), which is also what the greedy reflow in
;; transform.lisp's %wrap-line produces, so the oracle holds on either platform.
(define-row-case (23 "fmt -w 15 is transform --op reflow" :foreign ("fmt")) (ws)
  (expect-same-edit ws (format nil "one two three four~%~%five six seven~%") '("fmt")
                    "fmt -w 15 o.txt" (format nil "one two three~%four~%~%five six seven~%")
                    '("transform" "a.txt" "--op" "reflow" "--columns" "15")))

;;; Row 24

(define-row-case (24 "stripping trailing blanks with sed is transform --op strip-trailing" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "a  ~%b~C~%c~%" #\Tab) '("sed")
                    "sed -E 's/[[:blank:]]+$//' o.txt" (format nil "a~%b~%c~%")
                    '("transform" "a.txt" "--op" "strip-trailing")))

(define-row-case (24 "deleting blank lines with grep -v is transform --op delete-blank" :foreign ("grep")) (ws)
  (expect-same-edit ws (format nil "a~%~%  ~%b~%") '("grep")
                    "grep -v '^[[:space:]]*$' o.txt" (format nil "a~%b~%")
                    '("transform" "a.txt" "--op" "delete-blank")))

(define-row-case (24 "cat -s is transform --op squeeze-blank" :foreign ("cat")) (ws)
  (expect-same-edit ws (format nil "a~%~%~%~%b~%~%c~%") '("cat")
                    "cat -s o.txt" (format nil "a~%~%b~%~%c~%")
                    '("transform" "a.txt" "--op" "squeeze-blank")))

(define-row-case (24 "adding a missing final newline with perl is transform --op final-newline" :foreign ("perl")) (ws)
  (expect-same-edit ws (format nil "a~%b") '("perl")
                    "perl -0pe 's/\\n?\\z/\\n/' o.txt" (format nil "a~%b~%")
                    '("transform" "a.txt" "--op" "final-newline")))

(define-row-case (24 "dropping a BOM with tail -c +4 is transform --op strip-bom" :foreign ("tail")) (ws)
  (expect-same-edit ws (format nil "~Ca~%b~%" (code-char #xFEFF)) '("tail")
                    "tail -c +4 o.txt" (format nil "a~%b~%")
                    '("transform" "a.txt" "--op" "strip-bom")))

;;; Row 25

(define-row-case (25 "perl Unicode::Normalize NFC is transform --op nfc" :foreign ("perl")) (ws)
  (expect-same-edit ws (format nil "e~C and A~C~%" (code-char #x301) (code-char #x30A)) '("perl")
                    "perl -CSD -MUnicode::Normalize -pe '$_ = NFC($_)' o.txt"
                    (format nil "~C and ~C~%" (code-char #xE9) (code-char #xC5))
                    '("transform" "a.txt" "--op" "nfc")))

(define-row-case (25 "full-width to half-width with perl NFKC is transform --op nfkc" :foreign ("perl")) (ws)
  (expect-same-edit ws (format nil "~C~C~C ~C~%" (code-char #xFF21) (code-char #xFF11) (code-char #xFF71) (code-char #x2460)) '("perl")
                    "perl -CSD -MUnicode::Normalize -pe '$_ = NFKC($_)' o.txt"
                    (format nil "A1~C 1~%" (code-char #x30A2))
                    '("transform" "a.txt" "--op" "nfkc")))

;;; Row 26

(defparameter +japanese+ (format nil "あいう漢字カナ~%"))
(defparameter +japanese-sjis+
  (coerce #(130 160 130 162 130 164 138 191 142 154 131 74 131 105 10) '(vector (unsigned-byte 8))))
(defparameter +japanese-eucjp+
  (coerce #(164 162 164 164 164 166 180 193 187 250 165 171 165 202 10) '(vector (unsigned-byte 8))))

(define-row-case (26 "iconv -t SHIFT_JIS is transcode --to shift_jis" :foreign ("iconv")) (ws)
  (put ws "a.txt" +japanese+)
  (put ws "o.txt" +japanese+)
  (let ((expected (oracle ws '("iconv") "iconv -f UTF-8 -t SHIFT_JIS o.txt" +japanese-sjis+ :octets t)))
    (run-ok ws '("transcode" "a.txt" "--to" "shift_jis"))
    (expect (file-octets ws "a.txt") :to-equalp expected)))

(define-row-case (26 "iconv -t EUC-JP is transcode --to euc-jp" :foreign ("iconv")) (ws)
  (put ws "a.txt" +japanese+)
  (put ws "o.txt" +japanese+)
  (let ((expected (oracle ws '("iconv") "iconv -f UTF-8 -t EUC-JP o.txt" +japanese-eucjp+ :octets t)))
    (run-ok ws '("transcode" "a.txt" "--to" "euc-jp"))
    (expect (file-octets ws "a.txt") :to-equalp expected)))

(define-row-case (26 "nkf -s is transcode --to shift_jis" :foreign ("nkf")) (ws)
  (put ws "a.txt" +japanese+)
  (put ws "o.txt" +japanese+)
  (let ((expected (oracle ws '("nkf") "nkf -s o.txt" +japanese-sjis+ :octets t)))
    (run-ok ws '("transcode" "a.txt" "--to" "shift_jis"))
    (expect (file-octets ws "a.txt") :to-equalp expected)))

(define-row-case (26 "iconv -f SHIFT_JIS is read --encoding shift_jis" :foreign ("iconv")) (ws)
  (put ws "s.txt" +japanese-sjis+)
  (expect (read-text ws "s.txt" "--encoding" "shift_jis")
          :to-equal (oracle ws '("iconv") "iconv -f SHIFT_JIS -t UTF-8 s.txt" +japanese+)))

;;; Row 27

(defun pieces-text (workspace envelope)
  "Each file SPLIT created, in order, followed by a # line."
  (format nil "~{~A#~%~}"
          (mapcar (lambda (change) (file-text workspace (jget change "path")))
                  (jlist (jget envelope "changes")))))

(define-row-case (27 "split -l 2 is split --lines 2" :foreign ("split")) (ws)
  (put ws "a.txt" (format nil "1~%2~%3~%4~%5~%"))
  (put ws "o.txt" (format nil "1~%2~%3~%4~%5~%"))
  (let ((expected (oracle ws '("split") "split -l 2 o.txt p_ && for f in p_*; do cat \"$f\"; echo '#'; done"
                          (format nil "1~%2~%#~%3~%4~%#~%5~%#~%"))))
    (expect (pieces-text ws (run-ok ws '("split" "a.txt" "--lines" "2"))) :to-equal expected)))

(define-row-case (27 "csplit at a pattern is split --at-match" :foreign ("csplit")) (ws)
  (let ((content (format nil "intro~%## a~%x~%## b~%y~%")))
    (put ws "a.txt" content)
    (put ws "o.txt" content)
    (let ((expected (oracle ws '("csplit") "csplit -s -f q_ o.txt '/^## /' '{1}' && for f in q_*; do cat \"$f\"; echo '#'; done"
                            (format nil "intro~%#~%## a~%x~%#~%## b~%y~%#~%"))))
      (expect (pieces-text ws (run-ok ws '("split" "a.txt" "--at-match" "^## "))) :to-equal expected))))

;;; Row 28

(defun tree-snapshot (workspace directory)
  "Every entry under DIRECTORY as (relative kind mode content-or-target),
sorted: an observer independent of both aitools and the shell tools."
  (let ((root (ws-path workspace directory)))
    (labels ((walk (path prefix)
               (loop for entry in (sorted (mapcar (lambda (p)
                                                     (let ((name (if (uiop:directory-pathname-p p)
                                                                     (car (last (pathname-directory p)))
                                                                     (file-namestring p))))
                                                       name))
                                                   (append (uiop:subdirectories path)
                                                           (uiop:directory-files path))))
                     for relative = (if prefix (format nil "~A/~A" prefix entry) entry)
                     for native-path = (format nil "~A~A" (native path) entry)
                     for stat = (sb-posix:lstat native-path)
                     for mode = (sb-posix:stat-mode stat)
                     for kind = (cond ((= (logand mode #o170000) #o120000) :symlink)
                                      ((= (logand mode #o170000) #o040000) :dir)
                                      (t :file))
                     collect (list relative kind (logand mode #o7777)
                                   (case kind
                                     (:symlink (sb-posix:readlink native-path))
                                     (:file (read-octets native-path))))
                     when (eq kind :dir)
                       append (walk (uiop:ensure-directory-pathname native-path) relative))))
      (if (probe-file root) (walk root nil) :absent))))

(defun put-twin-trees (workspace)
  (dolist (side '("a" "o"))
    (put workspace (format nil "~A/f.txt" side) (format nil "file~%") :mode #o640)
    (put workspace (format nil "~A/dir/g.txt" side) (format nil "g~%"))
    (put workspace (format nil "~A/dir/sub/h.txt" side) (format nil "h~%"))
    (ensure-directories-exist (ws-path workspace (format nil "~A/emptydir/" side)))))

(defmacro define-file-operation-case ((row name foreign) script arguments)
  "Run the shell SCRIPT in o/ and aitools ARGUMENTS (paths under a/) on twin
trees, then compare the two trees entry by entry."
  `(define-row-case (,row ,name :foreign ,foreign) (ws)
     (put-twin-trees ws)
     (oracle ws ',foreign (format nil "cd o && ~A" ,script) nil)
     (run-ok ws ,arguments)
     (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/"))))

(define-file-operation-case (28 "cp is copy" ("cp"))
  "cp f.txt copy.txt" '("copy" "a/f.txt" "a/copy.txt"))

(define-file-operation-case (28 "cp -r is copy --recursive" ("cp"))
  "cp -r dir dir2" '("copy" "a/dir" "a/dir2" "--recursive"))

(define-file-operation-case (28 "mv is move" ("mv"))
  "mv f.txt moved.txt" '("move" "a/f.txt" "a/moved.txt"))

(define-file-operation-case (28 "mv dir new is move" ("mv"))
  "mv dir renamed" '("move" "a/dir" "a/renamed"))

(define-file-operation-case (28 "rm is delete" ("rm"))
  "rm f.txt" '("delete" "a/f.txt"))

(define-file-operation-case (28 "rmdir is delete on an empty directory" ("rmdir"))
  "rmdir emptydir" '("delete" "a/emptydir"))

;;; Row 29

(define-file-operation-case (29 "mkdir -p is mkdir" ("mkdir"))
  "mkdir -p new/deep/er" '("mkdir" "a/new/deep/er"))

(define-file-operation-case (29 "chmod +x is chmod --exec" ("chmod"))
  "chmod +x f.txt" '("chmod" "a/f.txt" "--exec"))

(define-file-operation-case (29 "chmod 644 is chmod --mode 0644" ("chmod"))
  "chmod 644 f.txt" '("chmod" "a/f.txt" "--mode" "0644"))

(define-file-operation-case (29 "ln -s is link" ("ln"))
  "ln -s f.txt f.link" '("link" "f.txt" "a/f.link"))

(define-row-case (29 "ln -sf on an existing symlink is link --overwrite" :foreign ("ln")) (ws)
  (put-twin-trees ws)
  (shell ws "ln -s f.txt a/l && ln -s f.txt o/l")
  (oracle ws '("ln") "cd o && ln -sf dir/g.txt l" nil)
  (run-ok ws (list "link" "dir/g.txt" "a/l" "--overwrite"
                   "--expect-hash" (format nil "a/l=~A" (info-hash ws "a/l"))))
  (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/")))

(define-row-case (29 "touch creates what touch creates" :foreign ("touch")) (ws)
  (put-twin-trees ws)
  (oracle ws '("touch") "cd o && touch new.txt" nil)
  (run-ok ws '("touch" "a/new.txt"))
  (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/")))

(define-row-case (29 "touch on an existing file moves its mtime forward as touch does" :foreign ("touch")) (ws)
  (put-twin-trees ws)
  (dolist (side '("a/f.txt" "o/f.txt"))
    (sb-posix:utimes (native (ws-path ws side)) 1577836800 1577836800))
  (oracle ws '("touch") "cd o && touch f.txt" nil)
  (run-ok ws '("touch" "a/f.txt"))
  (flet ((mtime (path) (sb-posix:stat-mtime (sb-posix:stat (native (ws-path ws path))))))
    (expect (> (mtime "a/f.txt") 1577836800) :to-be (> (mtime "o/f.txt") 1577836800))
    (expect (> (mtime "a/f.txt") 1577836800) :to-be t))
  (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/")))

(define-row-case (29 "mktemp's empty private file is mktemp's" :foreign ("mktemp")) (ws)
  (let* ((expected (oracle ws '("mktemp" "stat")
                           "f=$(mktemp) && { stat -f '%Lp %z' \"$f\" 2>/dev/null || stat -c '%a %s' \"$f\"; } && rm \"$f\""
                           (format nil "600 0~%")))
         (path (jget (run-ok ws '("mktemp")) "path"))
         (stat (sb-posix:stat path)))
    (expect (format nil "~O ~D~%" (logand #o7777 (sb-posix:stat-mode stat)) (sb-posix:stat-size stat))
            :to-equal expected)))

;;; Row 30

(define-row-case (30 "rename 's/\\.txt$/.md/' is find, then batch --atomic of moves" :foreign ("rename")) (ws)
  (dolist (name '("a.txt" "b.txt" "keep.log"))
    (put ws (format nil "a/~A" name) name)
    (put ws (format nil "o/~A" name) name))
  ;; The fixed expectation is the renamed tree itself, so without the Perl
  ;; rename it is built in o/ directly.
  (if (and (tool-path "rename")
           (zerop (nth-value 1 (shell ws "rename --help 2>&1 | grep -q 'perlexpr\\|perl'" :expect-status :any))))
      (oracle ws '("rename") "cd o && rename 's/\\.txt$/.md/' *.txt" nil)
      (progn
        (note-oracle :fixed '("rename (Perl)"))
        (dolist (name '("a" "b"))
          (rename-file (ws-path ws (format nil "o/~A.txt" name)) (ws-path ws (format nil "o/~A.md" name))))))
  (let* ((found (mapcar (lambda (item) (jget item "path"))
                        (jlist (jget (run-ok ws '("find" "*.txt" "a")) "items"))))
         (moves (mapcar (lambda (path)
                          (vector "move" path (format nil "~A.md" (subseq path 0 (- (length path) 4)))))
                        found)))
    (expect (length found) :to-be 2)
    (run-ok ws '("batch" "--atomic" "--stdin")
            :stdin (with-output-to-string (stream) (json-kit:write-json (coerce moves 'vector) stream)))
    (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/"))))
