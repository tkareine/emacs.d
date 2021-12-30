;; -*- lexical-binding: t; -*-

(require 'tk-init)

;; Enable backup files
(customize-set-variable 'make-backup-files t)

;; Make backups of files, even when they're in version control
(customize-set-variable 'vc-make-backup-files t)

;; Safer backups by copying files; preserves hard file links
(customize-set-variable 'backup-by-copying t)

;; Enable version control for backups
(customize-set-variable 'version-control t)

;; Number of newest backup versions to keep when making new backup
(customize-set-variable 'kept-new-versions 2)

;; Number of oldest backup versions to keep when making new backup
(customize-set-variable 'kept-old-versions 0)

;; Silently delete old backup files
(customize-set-variable 'delete-old-versions t)

;; Save all backup files to this directory
(customize-set-variable 'backup-directory-alist `(("." . ,(tk-init/user-emacs-path "backups"))))

;; Save all autosaves to this directory
(customize-set-variable 'auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Disable lockfiles, used to avoid editing collisions; these get placed
;; in the same directory as the original file, and there's no
;; configuration in Emacs to change the location. Lockfiles can be
;; harmful to tools monitoring a directory and detecting changes.
(customize-set-variable 'create-lockfiles nil)
