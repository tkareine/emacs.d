;; Enable backup files
(setq make-backup-files t)

;; Safer backup for symlinks
(setq backup-by-copying t)

;; Enable version control for backups
(setq version-control t)

;; Number of newest backup versions to keep when making new backup
(setq kept-new-versions 2)

;; Number of oldest backup versions to keep when making new backup
(setq kept-old-versions 2)

;; Silently delete old backup files
(setq delete-old-versions t)

;; Save all backup files to this directory
(setq backup-directory-alist `(("." . ,temporary-file-directory)))

;; Save all autosaves to this directory
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
