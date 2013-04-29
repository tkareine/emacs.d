;; Do not prettify lambda word with λ character (Emacs Starter Kit)
(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)

;; When killing, stop at subwords inside a CamelCase word
(add-hook 'prog-mode-hook 'subword-mode)

;; CSS language customizations
(setq css-indent-offset 2)

;; Erlang language support
(let* ((root-dir  (my-dotfile-lib-path "erlang-mode"))
       (bin-dir   (concat root-dir "/bin"))
       (elisp-dir (car (file-expand-wildcards (concat root-dir
                                                      "/lib/tools-2.6.*/emacs") t))))
  (when (and (file-exists-p bin-dir)
             (file-exists-p elisp-dir))
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
(setq js2-bounce-indent-p t)
(setq js2-concat-multiline-strings nil)
(setq js2-highlight-level 3)
(setq js2-missing-semi-one-line-override t)
(setq js2-strict-missing-semi-warning nil)
(custom-set-faces '(js2-private-member ((t (:foreground "coral1")))))

(add-hook 'js2-mode-hook
 (lambda ()
   (set-fill-column 300)))

;; Sass language support
(my-add-dotfile-lib-to-load-path "scss-mode")
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(setq scss-compile-at-save nil)

;; Scala language support
(my-add-dotfile-lib-to-load-path "scala-mode")
(require 'scala-mode-auto)

;; Textile language support
(require 'textile-mode)
