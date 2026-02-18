;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 26)
  (error "Emacs version >= 26 required (current is %s)" emacs-version))

(load (expand-file-name "site-lisp/tk-init" user-emacs-directory))

;; Avoid the following error when calling certain commands, such as
;; `package-install':
;;
;;   find-file-noselect-1: Maximum buffer size exceeded
;;
;; See `https://lists.nongnu.org/archive/html/bug-gnu-emacs/2025-08/msg01404.html'
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

(dolist (l '("tk-network"
             "tk-packages"
             "tk-looks"
             "tk-editing"
             "tk-dev"
             "tk-backups"))
  (load (tk-init/user-emacs-path "etc" l)))

;; Server mode
(server-start)

(message "Started in %.2fs, %d gcs"
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done)
