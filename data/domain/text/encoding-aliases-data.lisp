(in-package #:aitools.data)

(defparameter *encoding-aliases*
  '(("utf-8" . :utf-8) ("utf8" . :utf-8)
    ("shift_jis" . :shift_jis) ("shift-jis" . :shift_jis) ("sjis" . :shift_jis)
    ("cp932" . :shift_jis) ("windows-31j" . :shift_jis) ("ms932" . :shift_jis)
    ("euc-jp" . :euc-jp) ("eucjp" . :euc-jp) ("euc_jp" . :euc-jp)
    ("iso-8859-1" . :iso-8859-1) ("iso8859-1" . :iso-8859-1) ("latin1" . :iso-8859-1)
    ("latin-1" . :iso-8859-1)
    ("utf-16le" . :utf-16le) ("utf16le" . :utf-16le)
    ("utf-16be" . :utf-16be) ("utf16be" . :utf-16be)))

(export '*encoding-aliases*)
