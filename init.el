;; This file is based on @overtone's live-coding-emacs
;; <https://github.com/overtone/live-coding-emacs>. Thanks!

;; Create a variable to store the path to this dotfile directory
;; (usually ~/.emacs.d).
(defvar my-dotfile-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(defun my-dotfile-path (p)
  (concat my-dotfile-dir p))

(defun my-dotfile-etc-path (p)
  (concat (my-dotfile-path "etc/") p))

(defun my-dotfile-lib-path (p)
  (concat (my-dotfile-path "lib/") p))

(defun my-add-dotfile-to-load-path (p)
  (add-to-list 'load-path (my-dotfile-path p)))

(defun my-add-dotfile-lib-to-load-path (p)
  (add-to-list 'load-path (my-dotfile-lib-path p)))

(defun my-load-dotfile (f)
  (load-file (my-dotfile-path f)))

(defun my-load-dotfile-etc (f)
  (load-file (my-dotfile-etc-path f)))

(defun my-load-dotfile-lib (f)
  (load-file (my-dotfile-lib-path f)))

(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun my-require-packages-installed (lst)
  (let* ((missing-packages (my-filter '(lambda (x) (not (package-installed-p x))) lst)))
    (when missing-packages
      (error "Required packages are not installed: %s" missing-packages))))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(my-load-dotfile "dependencies.el")
(my-require-packages-installed my-package-dependencies)

(my-add-dotfile-to-load-path "lib")

(my-load-dotfile-etc "saves.el")
(my-load-dotfile-etc "keys.el")
(my-load-dotfile-etc "looks.el")
(my-load-dotfile-etc "programming.el")
(my-load-dotfile-etc "misc.el")
