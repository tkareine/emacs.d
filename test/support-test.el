(require 'ert)

(load-file (concat user-emacs-directory "etc/support.el"))

(ert-deftest tkareine/string-prefix-length-with-char-test ()
  (should (equal (tkareine/string-prefix-length-with-char ?a "") 0))
  (should (equal (tkareine/string-prefix-length-with-char ?a "b") 0))
  (should (equal (tkareine/string-prefix-length-with-char ?a "a") 1))
  (should (equal (tkareine/string-prefix-length-with-char ?a "aa") 2))
  (should (equal (tkareine/string-prefix-length-with-char ?a "ab") 1))
  (should (equal (tkareine/string-prefix-length-with-char ?a "aab") 2))
  (should (equal (tkareine/string-prefix-length-with-char ?a "ba") 0)))
