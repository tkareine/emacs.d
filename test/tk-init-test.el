;; -*- lexical-binding: t -*-

(require 'tk-init)

(ert-deftest tk-init/user-emacs-path-test ()
  (should (equal (tk-init/user-emacs-path) (expand-file-name "" user-emacs-directory)))
  (should (equal (tk-init/user-emacs-path "") (expand-file-name "" user-emacs-directory)))
(should (equal (tk-init/user-emacs-path "a") (expand-file-name "a" user-emacs-directory)))
  (should (equal (tk-init/user-emacs-path "a/b") (expand-file-name "a/b" user-emacs-directory)))
  (should (equal (tk-init/user-emacs-path "a" "b") (expand-file-name "a/b" user-emacs-directory)))
  (should (equal (tk-init/user-emacs-path "a" "b/c" "d/e") (expand-file-name "a/b/c/d/e" user-emacs-directory))))
