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
(customize-set-variable 'inhibit-startup-message t)

;; Set no content in *scratch* buffer
(customize-set-variable 'initial-scratch-message "")

;; Network security
(customize-set-variable 'network-security-level 'high)

;; Define rudimentary functions for loading the rest of init
(defun tkareine/dotfile-path (p)
  (concat user-emacs-directory p))

(defun tkareine/load-dotfile (f)
  (load-file (tkareine/dotfile-path f)))

;; Load support functions
(tkareine/load-dotfile "etc/support.el")

;; Start package system, make installed packages available
(require 'package)
(customize-set-variable 'package-archives '(("melpa" . "https://melpa.milkbox.net/packages/")
                                            ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Check that minimum set of packages is installed
(tkareine/load-dotfile "etc/packages.el")
(tkareine/add-selected-packages tkareine/package-dependencies)
(tkareine/install-missing-packages tkareine/package-dependencies)

;; Provide other libraries than packages
(tkareine/add-dotfile-to-load-path "lib")

;; Shell command to setup external files as required by Emacs
;; configuration.
(shell-command (mapconcat #'identity
                          `("touch ~/.cider_history"
                            "chmod 600 ~/.cider_history"
                            ,(concat "touch " (tkareine/dotfile-path ".custom.el")))
                          " && ")
               t
               nil)

;; Load configuration
(tkareine/load-dotfile "etc/minor-modes.el")
(tkareine/load-dotfile "etc/major-modes.el")
(tkareine/load-dotfile "etc/backups.el")
(tkareine/load-dotfile "etc/looks.el")
(tkareine/load-dotfile "etc/editing.el")
(tkareine/load-dotfile "etc/misc.el")

(setq custom-file (tkareine/dotfile-path ".custom.el"))
(load custom-file)

;; Server mode
(server-start)
