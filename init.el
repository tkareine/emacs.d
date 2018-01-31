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

;; Do not show startup message
(customize-set-variable 'inhibit-startup-echo-area-message (user-login-name))

;; Set no content in *scratch* buffer
(customize-set-variable 'initial-scratch-message "")

;; Provide other libraries than packages
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "lib"))

(require 'tk-support)

;; Shell command to setup external files as required by Emacs
;; configuration.
(shell-command (mapconcat #'identity
                          `("touch ~/.cider_history"
                            "chmod 600 ~/.cider_history"
                            ,(concat "touch " (tk-support/dotfile-path ".custom.el")))
                          " && ")
               t)

(setq custom-file (tk-support/dotfile-path ".custom.el"))
(load custom-file)

(dolist (l '("tk-network.el" "tk-packages.el"))
  (load-file (tk-support/dotfile-path "etc" l)))

;; Start package system, make installed packages available
(package-initialize)
(tk-packages/push-selected tk-packages/minimum-set)
(tk-packages/install-missing tk-packages/minimum-set)

(dolist (l '("tk-looks.el" "tk-dev.el" "tk-backups.el" "tk-editing.el"))
  (load-file (tk-support/dotfile-path "etc" l)))

;; Server mode
(server-start)

(message "Started in %s" (emacs-init-time))
