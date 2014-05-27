;; When killing, stop at subwords inside a CamelCase word
(add-hook 'prog-mode-hook 'subword-mode)

;; CSS language customizations
(setq css-indent-offset 2)

;; C language family customizations
(setq c-basic-offset 4)
(setq c-default-style "linux")

;; Erlang language support
(let* ((root-dir  (tkareine/dotfile-path "lib/erlang-mode"))
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
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq js2-concat-multiline-strings nil)
(setq js2-highlight-level 3)
(setq js2-missing-semi-one-line-override t)
(setq js2-strict-missing-semi-warning nil)
(custom-set-faces '(js2-private-member ((t (:foreground "coral1")))))

(add-hook 'js2-mode-hook (lambda () (set-fill-column 300)))
(add-hook 'js2-mode-hook 'flymake-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.javascript\\'" . js2-mode))

;; CoffeeScript support
(setq coffee-tab-width 2)

;; Sass language support
(setq scss-compile-at-save nil)

;; Scala language support
(tkareine/add-dotfile-to-load-path "lib/scala-mode")
(require 'scala-mode-auto)

;; Markdown file types
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Ruby file types
(add-to-list 'auto-mode-alist '("/gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("/guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("/procfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("/rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("/vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; Shell script file types
(add-to-list 'auto-mode-alist '("\\.bashrc\\." . shell-script-mode))

;; Textile language support
(require 'textile-mode)
