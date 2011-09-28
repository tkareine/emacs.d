;; This file is based on @overtone's live-coding-emacs
;; <https://github.com/overtone/live-coding-emacs>. Thanks!

;; Create a variable to store the path to this dotfile directory
;; (usually ~/.emacs.d).
(setq my-dotfiles-dir (file-name-directory
                       (or (buffer-file-name) load-file-name)))

(setq my-dotfiles-lib-dir (concat my-dotfiles-dir "lib/"))
(setq my-dotfiles-etc-dir (concat my-dotfiles-dir "etc/"))

(defun my-add-dotfile-path (p)
  (add-to-list 'load-path (concat my-dotfiles-dir p)))

(defun my-add-dotfile-lib-path (p)
  (add-to-list 'load-path (concat my-dotfiles-lib-dir p)))

(defun my-load-dotfile (f)
  (load-file (concat my-dotfiles-dir f)))

(defun my-load-dotfile-etc (f)
  (load-file (concat my-dotfiles-etc-dir f)))

(defun my-load-dotfile-lib (f)
  (load-file (concat my-dotfiles-lib-dir f)))

(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun my-require-packages-installed (lst)
  (let* ((missing-packages (my-filter '(lambda (x) (not (package-installed-p x))) lst)))
    (when missing-packages
      (error "Required packages are not installed: %s" missing-packages))))

;; Start ELPA
(when (load (concat my-dotfiles-dir "elpa/package.el"))
  (package-initialize))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(my-load-dotfile "dependencies.el")
(my-require-packages-installed my-package-dependencies)

(my-add-dotfile-path "lib")

(my-load-dotfile-etc "backups.el")
(my-load-dotfile-etc "keys.el")
(my-load-dotfile-etc "looks.el")
(my-load-dotfile-etc "programming.el")
(my-load-dotfile-etc "misc.el")
