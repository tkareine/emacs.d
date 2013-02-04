;; This file is based on @overtone's live-coding-emacs
;; <https://github.com/overtone/live-coding-emacs>. Thanks!

;; Configuration that must be done early

;; Disable mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Do not show splash screen
(setq inhibit-startup-message t)

;; Create a variable to store the path to this dotfile directory
;; (usually ~/.emacs.d)
(defvar my-dotfile-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(defun my-dotfile-path (p)
  (concat my-dotfile-dir p))

(defun my-dotfile-etc-path (p)
  (concat (my-dotfile-path "etc/") p))

(defun my-dotfile-lib-path (p)
  (concat (my-dotfile-path "lib/") p))

(defun my-add-dotfile-to-load-path (p)
  (add-to-list 'load-path (my-dotfile-path p)))

(defun my-add-dotfile-lib-to-load-path (p)
  (add-to-list 'load-path (my-dotfile-lib-path p)))

(defun my-load-dotfile (f)
  (load-file (my-dotfile-path f)))

(defun my-load-dotfile-etc (f)
  (load-file (my-dotfile-etc-path f)))

(defun my-load-dotfile-lib (f)
  (load-file (my-dotfile-lib-path f)))

(my-load-dotfile-etc "functions.el")

;; Start package system, make installed packages available
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Check that minimum set of packages is installed
(my-load-dotfile-etc "dependencies.el")
(my-require-packages-installed my-package-dependencies)

;; Provide other libraries than packages
(my-add-dotfile-to-load-path "lib")

;; Load configuration
(my-load-dotfile-etc "saves.el")
(my-load-dotfile-etc "looks.el")
(my-load-dotfile-etc "programming.el")
(my-load-dotfile-etc "misc.el")
(my-load-dotfile-etc "keys.el")
