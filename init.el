;; -*- lexical-binding: t; -*-

(load (expand-file-name "site-lisp/tk-init" user-emacs-directory))

(setq custom-file (tk-init/user-emacs-path "custom.el"))
(load custom-file t)

(dolist (l '("tk-internals"
             "tk-network"
             "tk-packages"
             "tk-looks"
             "tk-dev"
             "tk-backups"
             "tk-editing"))
  (load (tk-init/user-emacs-path "etc" l)))

;; Server mode
(server-start)

(message "Started in %s" (emacs-init-time))
