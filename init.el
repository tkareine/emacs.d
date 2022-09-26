;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 26)
  (error "Emacs version >= 26 required (current is %s)" emacs-version))

(load (expand-file-name "site-lisp/tk-init" user-emacs-directory))

(setq-default custom-file null-device)

(dolist (l '("tk-network"
             "tk-packages"
             "tk-looks"
             "tk-dev"
             "tk-backups"
             "tk-editing"))
  (load (tk-init/user-emacs-path "etc" l)))

;; Server mode
(server-start)

(message "Started in %.2fs, %d gcs"
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done)
