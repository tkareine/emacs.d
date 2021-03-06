;; -*- lexical-binding: t -*-

(require 'tk-support)

(ert-deftest tk-support/locate-any-dominating-file-test ()
  (should-not (tk-support/locate-any-dominating-file (concat user-emacs-directory "test") '("no-such-name")))
  (should (equal (tk-support/locate-any-dominating-file (concat user-emacs-directory "test") '("no-such-name" "init.el"))
                 (concat (expand-file-name user-emacs-directory) "init.el"))))

(ert-deftest tk-support/npm-global-path-test ()
  (should (string-match-p "/\\.nodes/node-[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+/lib/node_modules/foo\\.js\\'" (tk-support/npm-global-path "foo.js")))
  (should (string-match-p "/\\.nodes/node-[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+/lib/node_modules/foo/bar\\.js\\'" (tk-support/npm-global-path "foo/bar.js")))
  (should (string-match-p "/\\.nodes/node-[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+/lib/node_modules/foo/bar\\.js\\'" (tk-support/npm-global-path "foo" "bar.js"))))

(ert-deftest tk-support/string-prefix-length-with-char-test ()
  (should (equal (tk-support/string-prefix-length-with-char ?a "") 0))
  (should (equal (tk-support/string-prefix-length-with-char ?a "b") 0))
  (should (equal (tk-support/string-prefix-length-with-char ?a "a") 1))
  (should (equal (tk-support/string-prefix-length-with-char ?a "aa") 2))
  (should (equal (tk-support/string-prefix-length-with-char ?a "ab") 1))
  (should (equal (tk-support/string-prefix-length-with-char ?a "aab") 2))
  (should (equal (tk-support/string-prefix-length-with-char ?a "ba") 0)))
