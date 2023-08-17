;; -*- lexical-binding: t; -*-

(defun tk-packages/upgrade-packages ()
  (interactive)
  (let ((package-menu-async nil))
    (message ";;; Updating package list…\n")
    (package-list-packages)

    (message "\n;;; Upgrading packages (if any)…\n")
    (package-menu-mark-upgrades)
    (ignore-errors (package-menu-execute t))))

(defun tk-packages/recompile-packages ()
  (interactive)
  (byte-recompile-directory package-user-dir 0 'force))

;; We call `package-initialize' ourselves
(setq-default package-enable-at-startup nil)

(setq-default package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                                 ("melpa"        . "https://melpa.org/packages/")
                                 ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Start package system, make installed packages available (activation)
(package-initialize)

;; Docs: https://jwiegley.github.io/use-package/
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
   tk-support/pretty-print-xml
   tk-support/string-prefix-length-with-char)

  :load-path
  "site-lisp/tk-support")
