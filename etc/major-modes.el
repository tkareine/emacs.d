;; When killing, stop at subwords inside a CamelCase word
(add-hook 'prog-mode-hook #'subword-mode)

;; Use text-mode for *scratch* buffer
(customize-set-variable 'initial-major-mode 'text-mode)

;; CSS language customizations
(setq css-indent-offset 2)

;; C language family customizations
(setq c-basic-offset 4)
(setq c-default-style "linux")

;; Ediff: use current frame for control panel
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Ediff: I find it easier to read a diff by having one buffer on the
;; left and another on the right
(setq ediff-split-window-function 'split-window-horizontally)

;; Erlang language support
(let* ((root-dir  (tkareine/dotfile-path "lib/erlang-mode"))
       (bin-dir   (concat root-dir "/bin"))
       (elisp-dir (car (file-expand-wildcards (concat root-dir
                                                      "/lib/tools-*/emacs") t))))
  (when (and (file-exists-p bin-dir)
             elisp-dir
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
(global-set-key (kbd "C-c j")          #'tkareine/js2-mode-toggle-strict-missing-semi-warning)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.javascript\\'" . js2-mode))

;; CoffeeScript support
(setq coffee-tab-width 2)

;; Haskell support
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-process-type 'cabal-repl)
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (turn-on-haskell-decl-scan)
            (interactive-haskell-mode)
            (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
            (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
            (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
            (define-key haskell-mode-map (kbd "C-c o") 'haskell-hoogle)
            (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

;; Sass language support
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))

;; Scala language support
(tkareine/add-dotfile-to-load-path "lib/scala-mode")
(require 'scala-mode-auto)

;; Markdown language support
(customize-set-variable 'markdown-command "marked --gfm --tables")
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

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

;; Visual Basic support
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.aspx\\'" . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.bas\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.cls\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.frm\\'" . markdown-mode))
