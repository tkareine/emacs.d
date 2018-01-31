(require 'tk-support)

;; Enable backup files
(customize-set-variable 'make-backup-files t)

;; Make backups of files, even when they're in version control
(customize-set-variable 'vc-make-backup-files t)

;; Safer backup for symlinks
(customize-set-variable 'backup-by-copying t)

;; Enable version control for backups
(customize-set-variable 'version-control t)

;; Number of newest backup versions to keep when making new backup
(customize-set-variable 'kept-new-versions 2)

;; Number of oldest backup versions to keep when making new backup
(customize-set-variable 'kept-old-versions 2)

;; Silently delete old backup files
(customize-set-variable 'delete-old-versions t)

;; Save all backup files to this directory
(customize-set-variable 'backup-directory-alist `(("." . ,(tk-support/dotfile-path "backups"))))

;; Save all autosaves to this directory
(customize-set-variable 'auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
