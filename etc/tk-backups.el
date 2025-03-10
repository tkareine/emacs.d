;; -*- lexical-binding: t; -*-

(require 'tk-init)

;; Enable backup files
(setq-default make-backup-files t)

;; Make backups of files, even when they're in version control
(setq-default vc-make-backup-files nil)

;; Safer backups by copying files; preserves hard file links
(setq-default backup-by-copying t)

;; Enable version control for backups
(setq-default version-control t)

;; Number of newest backup versions to keep when making new backup
(setq-default kept-new-versions 2)

;; Number of oldest backup versions to keep when making new backup
(setq-default kept-old-versions 0)

;; Silently delete old backup files
(setq-default delete-old-versions t)

;; Backup a file that satisfies both the following conditions:
;;
;; 1. The file satisfies the normal (default) predicate of Emacs
;; 2. The file is not a remote file
(defun tk-backups/backup-file-enable-p (name)
  (and (not (file-remote-p name 'method))
       (normal-backup-enable-predicate name)))

(setq-default backup-enable-predicate #'tk-backups/backup-file-enable-p)

;; Save all backup files to this directory
(setq-default backup-directory-alist `(("." . ,(tk-init/user-emacs-path "backups"))))

;; Save all autosaves to this directory
(setq-default auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Disable lockfiles, used to avoid editing collisions; these get placed
;; in the same directory as the original file, and there's no
;; configuration in Emacs to change the location. Lockfiles can be
;; harmful to tools monitoring a directory and detecting changes.
(setq-default create-lockfiles nil)
