;; -*- lexical-binding: t; -*-

(defun tk-packages/upgrade-packages ()
  (let ((package-menu-async nil))
    (message ";;; Updating package list…\n")
    (package-list-packages)

    (message "\n;;; Upgrading packages (if any)…\n")
    (package-menu-mark-upgrades)
    (ignore-errors (package-menu-execute t))))

;; We call `package-initialize' ourselves.
(customize-set-variable 'package-enable-at-startup nil)

(customize-set-variable 'package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                                            ("melpa"        . "https://melpa.org/packages/")
                                            ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Start package system, make installed packages available (activation)
(package-initialize)

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package))

(use-package bind-key)

(use-package tk-support
  :commands
  (tk-support/active-region-or-line
   tk-support/locate-any-dominating-file
   tk-support/npm-global-path
   tk-support/pretty-print-xml
   tk-support/string-prefix-length-with-char)

  :load-path
  "site-lisp/tk-support")
