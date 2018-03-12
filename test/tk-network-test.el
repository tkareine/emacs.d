;; -*- lexical-binding: t -*-

(require 'ert)
(require 'tk-support)

(load-file (tk-support/dotfile-path "etc" "tk-network.el"))

(ert-deftest tk-network/tls-security-test ()
  (should (tk-network/test-tls-security)))
