;; This file is originally based on @overtone's live-coding-emacs
;; <https://github.com/overtone/live-coding-emacs>. Thanks!

;; Configuration that must be done early

;; Hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Hide tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Hide scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Do not show splash screen
(setq inhibit-startup-message t)

;; Set no content in *scratch* buffer
(setq initial-scratch-message "")

;; Define rudimentary functions for loading the rest of init
(defun tkareine/dotfile-path (p)
  (concat user-emacs-directory p))

(defun tkareine/load-dotfile (f)
  (load-file (tkareine/dotfile-path f)))

;; Load the rest of my functions
(tkareine/load-dotfile "etc/functions.el")

;; Start package system, make installed packages available
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Check that minimum set of packages is installed
(tkareine/load-dotfile "etc/dependencies.el")
(tkareine/require-packages-installed tkareine/package-dependencies)

;; Provide other libraries than packages
(tkareine/add-dotfile-to-load-path "lib")

;; Load configuration
(tkareine/load-dotfile "etc/minor-modes.el")
(tkareine/load-dotfile "etc/major-modes.el")
(tkareine/load-dotfile "etc/backups.el")
(tkareine/load-dotfile "etc/looks.el")
(tkareine/load-dotfile "etc/bindings.el")
(tkareine/load-dotfile "etc/misc.el")
