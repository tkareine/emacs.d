;; -*- lexical-binding: t; -*-

(require 'package)

(defun tk-packages/upgrade-packages ()
  (let ((package-menu-async nil))
    (message ";;; Updating package list…\n")
    (package-list-packages)

    (message "\n;;; Upgrading packages (if any)…\n")
    (package-menu-mark-upgrades)
    (ignore-errors (package-menu-execute t))))

(customize-set-variable 'package-enable-at-startup nil)

(customize-set-variable 'package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                                            ("melpa"        . "https://melpa.org/packages/")
                                            ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Start package system, make installed packages available
(package-initialize)

(eval-when-compile
  (require 'use-package))
