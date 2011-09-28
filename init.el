;; This file is based upon @overtone's live-coding-emacs
;; <https://github.com/overtone/live-coding-emacs>. Thanks!

;; Create a variable to store the path to this dotfile directory
;; (usually ~/.emacs.d).
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq dotfiles-lib-dir (concat dotfiles-dir "lib/"))
(setq dotfiles-etc-dir (concat dotfiles-dir "etc/"))

(defun add-dotfile-path (p)
  (add-to-list 'load-path (concat dotfiles-dir p)))

(defun add-dotfile-lib-path (p)
  (add-to-list 'load-path (concat dotfiles-lib-dir p)))

(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

(defun load-dotfile-etc (f)
  (load-file (concat dotfiles-etc-dir f)))

(defun load-dotfile-lib (f)
  (load-file (concat dotfiles-lib-dir f)))

;; Start ELPA
(when (load (concat dotfiles-dir "elpa/package.el"))
  (package-initialize))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-dotfile-path "lib")

(load-dotfile-etc "keys.el")
(load-dotfile-etc "looks.el")
(load-dotfile-etc "programming.el")
(load-dotfile-etc "misc.el")
