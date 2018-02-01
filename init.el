;; This file is originally based on @overtone's live-coding-emacs
;; <https://github.com/overtone/live-coding-emacs>. Thanks!

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
