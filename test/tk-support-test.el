;; -*- lexical-binding: t -*-

(require 'tk-init)
(require 'tk-support)

(ert-deftest tk-support/locate-any-dominating-file-test ()
  (should-not (tk-support/locate-any-dominating-file (concat user-emacs-directory "test") '("no-such-name")))
  (should (equal (tk-support/locate-any-dominating-file (concat user-emacs-directory "test") '("no-such-name" "init.el"))
                 (concat (expand-file-name user-emacs-directory) "init.el"))))

(ert-deftest tk-support/string-prefix-length-with-char-test ()
  (should (equal (tk-support/string-prefix-length-with-char ?a "") 0))
  (should (equal (tk-support/string-prefix-length-with-char ?a "b") 0))
  (should (equal (tk-support/string-prefix-length-with-char ?a "a") 1))
  (should (equal (tk-support/string-prefix-length-with-char ?a "aa") 2))
  (should (equal (tk-support/string-prefix-length-with-char ?a "ab") 1))
  (should (equal (tk-support/string-prefix-length-with-char ?a "aab") 2))
  (should (equal (tk-support/string-prefix-length-with-char ?a "ba") 0)))

(ert-deftest tk-support/unfill-paragraph--when-point-at-min ()
  (with-temp-buffer
    (insert "A paragraph\nthat spans several\nlines of text.")
    (goto-char (point-min))
    (tk-support/unfill-paragraph)
    (should (equal (buffer-string)
                   "A paragraph that spans several lines of text."))))

(ert-deftest tk-support/unfill-paragraph--when-point-at-middle-of-paragraph ()
  (with-temp-buffer
    (insert "Alpha beta\ngamma delta\nepsilon zeta.")
    (goto-char (point-min))
    (forward-line 1) ; Point now on the second line
    (tk-support/unfill-paragraph)
    (should (equal (buffer-string)
                   "Alpha beta gamma delta epsilon zeta."))))

(ert-deftest tk-support/unfill-paragraph--only-affects-current-paragraph ()
  (with-temp-buffer
    (insert "First paragraph\nfirst continues.\n\nSecond paragraph\nsecond continues.")
    (goto-char (point-min))
    (tk-support/unfill-paragraph)
    (should (equal (buffer-string)
                   "First paragraph first continues.\n\nSecond paragraph\nsecond continues."))))

(ert-deftest tk-support/unfill-paragraph--noop-when-already-single-line ()
  (with-temp-buffer
    (insert "Already a single line of text.")
    (goto-char (point-min))
    (tk-support/unfill-paragraph)
    (should (equal (buffer-string)
                   "Already a single line of text."))))

(ert-deftest tk-support/unfill-paragraph--when-region-spans-multiple-paragraphs ()
  (with-temp-buffer
    (insert "First paragraph\nfirst continues.\n\nSecond paragraph\nsecond continues.")
    (transient-mark-mode 1)
    (push-mark (point-min) t)
    (goto-char (point-max))
    (activate-mark)
    (tk-support/unfill-paragraph t)
    (should (equal (buffer-string)
                   "First paragraph first continues.\n\nSecond paragraph second continues."))))

(ert-deftest tk-support/unfill-paragraph--signal-on-read-only-buffer ()
  (with-temp-buffer
    (insert "Some text\nto unfill.")
    (setq buffer-read-only t)
    (should-error (call-interactively #'tk-support/unfill-paragraph)
                  :type 'buffer-read-only)))

(ert-deftest tk-support/xml-pretty-print-test ()
  (ert-test-erts-file (tk-init/user-emacs-path "test" "tk-support-xml-pretty-print.erts")
                      (lambda () (call-interactively #'tk-support/xml-pretty-print))))
