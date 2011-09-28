;; Enable backup files
(setq make-backup-files t)

;; Safer backup for symlinks
(setq backup-by-copying t)

;; Enable version control for backups
(setq version-control t)

;; Save all backup files in this directory
(setq backup-directory-alist `(".*" . (concat dotfiles-dir "backups/"))

;; Silently delete old backup files
(setq delete-old-versions t)
