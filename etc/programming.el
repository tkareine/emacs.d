;; CSS language customizations
(setq css-indent-offset 2)

;; Erlang language support
(let* ((root-dir  (my-dotfile-lib-path "erlang-mode"))
       (bin-dir   (concat root-dir "/bin"))
       (elisp-dir (car (file-expand-wildcards (concat root-dir
                                                      "/lib/tools-2.6.*/emacs") t))))
  (when (and (file-exists-p bin-dir)
             (file-exists-p elisp-dir))
    (message "erlang start")
    (setq erlang-root-dir root-dir)
    (add-to-list 'load-path elisp-dir)
    (add-to-list 'exec-path bin-dir)
    (require 'erlang-start)))

;; JavaScript language support
(my-add-dotfile-lib-to-load-path "js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-missing-semi-one-line-override t)

(defun my-toggle-js2-strict-missing-semi-warning ()
  (interactive)
  (setq js2-strict-missing-semi-warning (eq js2-strict-missing-semi-warning nil))
  (js2-mode))

(add-hook 'js2-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c s") 'my-toggle-js2-strict-missing-semi-warning)))

;; Sass language support
(my-add-dotfile-lib-to-load-path "scss-mode")
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(setq scss-compile-at-save nil)

;; Scala language support
(my-add-dotfile-lib-to-load-path "scala-mode")
(require 'scala-mode-auto)
