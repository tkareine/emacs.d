;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 26)
  (error "Emacs version >= 26 required (current is %s)" emacs-version))

(load (expand-file-name "site-lisp/tk-init" user-emacs-directory))

(setq custom-file "/dev/null")
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
