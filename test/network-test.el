(require 'ert)

(load-file (concat user-emacs-directory "etc/network.el"))

(ert-deftest tkareine/tls-security-test ()
  (should (tkareine/test-tls-security)))
