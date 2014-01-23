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

;; Rudimentary helpers for loading the rest of init
(defun my-dotfile-path (p)
  (concat user-emacs-directory p))

(defun my-load-dotfile (f)
  (load-file (my-dotfile-path f)))

(defun my-add-dotfile-to-load-path (p)
  (add-to-list 'load-path (my-dotfile-path p)))

(my-load-dotfile "etc/functions.el")

;; Start package system, make installed packages available
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Check that minimum set of packages is installed
(my-load-dotfile "etc/dependencies.el")
(my-require-packages-installed my-package-dependencies)

;; Provide other libraries than packages
(my-add-dotfile-to-load-path "lib")

;; Load configuration
(my-load-dotfile "etc/minor-modes.el")
(my-load-dotfile "etc/major-modes.el")
(my-load-dotfile "etc/backups.el")
(my-load-dotfile "etc/looks.el")
(my-load-dotfile "etc/bindings.el")
(my-load-dotfile "etc/misc.el")
