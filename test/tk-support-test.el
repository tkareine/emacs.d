;; -*- lexical-binding: t -*-

(require 'ert)
(require 'tk-support)

(ert-deftest tk-support/dotfiles-path-test ()
  (should (equal (tk-support/dotfile-path "") (concat (expand-file-name user-emacs-directory))))
  (should (equal (tk-support/dotfile-path "a") (concat (expand-file-name user-emacs-directory) "a")))
  (should (equal (tk-support/dotfile-path "a/b") (concat (expand-file-name user-emacs-directory) "a/b")))
  (should (equal (tk-support/dotfile-path "a" "b") (concat (expand-file-name user-emacs-directory) "a/b"))))

(ert-deftest tk-support/string-prefix-length-with-char-test ()
  (should (equal (tk-support/string-prefix-length-with-char ?a "") 0))
  (should (equal (tk-support/string-prefix-length-with-char ?a "b") 0))
  (should (equal (tk-support/string-prefix-length-with-char ?a "a") 1))
  (should (equal (tk-support/string-prefix-length-with-char ?a "aa") 2))
  (should (equal (tk-support/string-prefix-length-with-char ?a "ab") 1))
  (should (equal (tk-support/string-prefix-length-with-char ?a "aab") 2))
  (should (equal (tk-support/string-prefix-length-with-char ?a "ba") 0)))
