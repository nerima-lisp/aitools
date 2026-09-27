;;;; t/e2e/correspondence-rows.lisp
;;;;
;;;; The rows of the table of shell operations and their aitools
;;;; replacements, one string per row in table order (row N is element N-1).
;;;; The e2e cases name the row they cover by number; the meta specs in
;;;; meta-test.lisp check that every row has a case and no case names a row
;;;; that does not exist.
(in-package #:aitools.e2e.test)

(defparameter *correspondence-rows*
  (quote
   ("| `cat`、`head -n`、`tail -n`、`sed -n 'S,Ep'`、`nl` | `read --range`、`read --tail` |"
    "| `sed -n '/A/,/B/p'` | `read --between A B` |"
    "| `cat -A`、不可視文字やゼロ幅文字の確認 | `read --escape-invisible` |"
    "| `xxd`、`od`、`hexdump`、`strings` | `read --as hex`、`read --as strings` |"
    "| `wc`、`stat`、`file`、`sha256sum`、`md5sum`、`test -e`、`realpath`、`readlink -f` | `info`、`info --digest`、`info --allow-missing` |"
    "| `grep`、`grep -r`、`rg`、`grep -v`、`grep -x`、`grep -e A -e B` | `search`、`--invert`、`--line-regexp`、`--pattern` |"
    "| `grep -c`、`grep -l`、`grep -L`、`grep -o`、`grep -oP` | `search --output count\\|files\\|files-without-match\\|matches` |"
    "| `find`、`fd`、`ls -la`、`ls -R`、`tree`、`du` | `find`、`find --depth 1`、`find --output tree`、`find --sizes` |"
    "| `find -empty`、`find -perm +x`、`find -type l`、`find -newer` | `find --empty`、`--executable`、`--type symlink`、`--newer` |"
    "| `ctags`、定義や参照の grep | `code outline`、`code defs`、`code refs` |"
    "| `diff -u`、`diff -w`、`cmp`、`comm` | `diff`、`--ignore-whitespace`、`identical`、`diff --output set` |"
    "| `sed -i 's/a/b/'`（1 箇所） | `edit --old a --new b` |"
    "| `sed -i 's/…/…/g'`、`perl -pi -e 's/…/…/g'`、`sed 's/x/y/2'` | `replace … --expect-count N`、`replace --nth 2` |"
    "| `perl -0pi -e 's/…\\n…/…/s'`、`paste -sd,` | `replace --multiline` |"
    "| `perl -pi -e 's/(\\w+)/\\U$1/'`、番号の繰り上げ | `replace` の `${1:upper}`、`${1:inc}` |"
    "| `sed -i 'S,Es/…/…/'` | `replace --range S:E` |"
    "| `sed -i 'Nd'`、`sed -i 'Nc text'` | `edit --range N --new ''`、`edit --range N --new text` |"
    "| `sed -i '/re/d'`、`grep -v re > tmp && mv` | `edit --match re --new '' --expect-count N` |"
    "| `sed -i '/re/a text'`、`sed -i '/re/i text'`、`sed -i '1i text'` | `insert --after --match re`、`insert --before --match re`、`insert --at start` |"
    "| `cat > file <<EOF`、`echo >> file`、`tee`、`cat a b > c` | `write --stdin`、`insert --at end`、`write --content-file a --content-file b` |"
    "| `patch`、`git apply`、`patch -R`、`patch -p1` | `apply`、`apply --reverse`、`apply --strip 1` |"
    "| `sort`、`sort -n`、`sort -k2`、`sort -V`、`sort -u`、`uniq`、`tac`、`shuf` | `transform --op sort\\|sort-numeric\\|sort-version\\|unique\\|reverse\\|shuffle`、`--key` |"
    "| `tr a-z A-Z`、`dos2unix`、`expand`、`unexpand`、`fold -s`、`fmt` | `transform --op upper\\|eol-lf\\|tabs-to-spaces\\|spaces-to-tabs\\|wrap\\|reflow` |"
    "| 行末空白、空行、末尾改行、BOM の整理 | `transform --op strip-trailing\\|delete-blank\\|squeeze-blank\\|final-newline\\|strip-bom` |"
    "| `perl -MUnicode::Normalize`、全角と半角の統一 | `transform --op nfc\\|nfkc` |"
    "| `iconv`、`nkf` | `transcode`、`read --encoding` |"
    "| `split -l`、`csplit` | `split --lines`、`split --at-match` |"
    "| `cp`、`cp -r`、`mv`、`mv dir new`、`rm`、`rmdir` | `copy`、`copy --recursive`、`move`、`delete` |"
    "| `mkdir -p`、`chmod +x`、`chmod 644`、`ln -s`、`ln -sf`、`touch`、`mktemp` | `mkdir`、`chmod --exec`、`chmod --mode 0644`、`link`、`link --overwrite`、`touch`、`mktemp` |"
    "| `rename 's/…/…/'` | `find` で対象を得て、`move` を並べた `batch --atomic` |"
    "| `jq '.a.b'`、`jq 'keys'`、`jq 'length'`、`jq -r` | `json get`、`--keys`、`length`、`--raw` |"
    "| `jq '.[] \\| select(.x == 1)'`、`jq 'sort_by(.x)'`、`jq 'map(…) \\| length'` | `json select --where`、`--sort-by`、`--output count` |"
    "| `jq '.a = 1'`、`jq '.xs += [1]'`、`jq 'del(.a)'`、`jq '. * {…}'`、`jq .` | `json set`、`json set /xs/-`、`json delete`、`json merge`、`json fmt` |"
    "| `cut -d: -f1`、`awk -F: '{print $1}'`、`awk '{print $3}'` | `table read --format sep --delimiter ':' --no-header --columns 1`、`table read --format ws --no-header --columns 3` |"
    "| `awk '{s+=$2} END {print s}'`、`sort \\| uniq -c \\| sort -rn`、`uniq -d` | `table agg --sum`、`table agg --format lines --group-by line --count --sort count --desc`、`--min-count 2` |"
    "| `tar -tf`、`unzip -l`、`zcat`、`unzip -p`、`tar -xOf` | `archive list`、`archive read` |"
    "| `tar -xzf`、`unzip`、`tar -czf`、`zip -r`、`gzip` | `archive extract --to`、`archive create` |"
    "| `cmd \\| grep`、`cmd \\| head`、`cmd \\| tail`、`cmd > file`、ANSI 色の除去 | `run --grep`、`--head`、`--tail`、`--stdout-to`、既定の ANSI 除去 |"
    "| `cmd &`、`nohup`、`tail -f`、`sleep`、ポートの待機、`timeout` | `bg start`、`bg logs --from`、`wait`、`run --timeout` |"
    "| `git status`、`git log`、`git diff`、`git blame`、`git show rev:path` | `git status`、`git log`、`git diff`、`git blame`、`git show` |"
    "| `uname`、`whoami`、`hostname`、`nproc`、`env`、`which`、`ps`、`lsof -i` | `sys info`、`sys env`、`sys tools`、`sys procs`、`sys ports` |"
    "| `date`、`date -d '+1 day'`、`TZ=… date`、2 時刻の差 | `time now`、`time convert --add 1d`、`--tz`、`time diff` |"
    "| `base64`、`base64 -d > file`、`xxd -r -p`、URL エンコード | `util encode`、`util decode --to`、`util encode url` |"
    "| `bc`、`expr`、`$((…))`、`python3 -c 'print(…)'` | `util calc` |"
    "| `uuidgen`、`openssl rand` | `util uuid`、`util random` |"))
  "The rows of the correspondence table, in table order.")
