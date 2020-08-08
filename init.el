;; -*- lexical-binding: t; -*-

;; Provide other libraries than packages
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "lib"))

(require 'tk-support)

(setq custom-file (tk-support/dotfile-path "custom.el"))
(load custom-file t)

(dolist (l '("tk-internals.el"
             "tk-network.el"
             "tk-packages.el"
             "tk-looks.el"
             "tk-dev.el"
             "tk-backups.el"
             "tk-editing.el"))
  (load-file (tk-support/dotfile-path "etc" l)))

;; Server mode
(server-start)

(message "Started in %s" (emacs-init-time))
