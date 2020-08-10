;; -*- lexical-binding: t -*-

(require 'tk-init)

(load-file (tk-init/user-emacs-path "etc" "tk-network.el"))

(ert-deftest tk-network/tls-security-test ()
  (should (tk-network/test-tls-security)))
