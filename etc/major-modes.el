;; When killing, stop at subwords inside a CamelCase word
(add-hook 'prog-mode-hook #'subword-mode)

;; Use text-mode for *scratch* buffer
(customize-set-variable 'initial-major-mode 'text-mode)

;; CSS language customizations
(customize-set-variable 'css-indent-offset 2)

;; C language family customizations
(customize-set-variable 'c-basic-offset 4)
(customize-set-variable 'c-default-style '((awk-mode  . "awk")
                                           (java-mode . "java")
                                           (other     . "linux")))

;; Ediff: use current frame for control panel
(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

;; Ediff: I find it easier to read a diff by having one buffer on the
;; left and another on the right
(customize-set-variable 'ediff-split-window-function 'split-window-horizontally)

;; JavaScript language support
(customize-set-variable 'js-indent-level 2)
(customize-set-variable 'js2-basic-offset 2)
(customize-set-variable 'js2-bounce-indent-p t)
(customize-set-variable 'js2-concat-multiline-strings nil)
(customize-set-variable 'js2-highlight-level 3)
(customize-set-variable 'js2-missing-semi-one-line-override t)
(customize-set-variable 'js2-strict-missing-semi-warning nil)

(advice-add #'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args))) ; Don't double-indent multiline statement

(custom-set-faces '(js2-private-member ((t (:foreground "coral1")))))

(defun tkareine/js2-mode-toggle-strict-missing-semi-warning ()
  (interactive)
  (setq js2-strict-missing-semi-warning (eq js2-strict-missing-semi-warning nil))
  (js2-mode))

(defun tkareine/js2-mode-customizations ()
  (define-key js2-mode-map (kbd "M-.")   nil)
  (define-key js2-mode-map (kbd "C-c j") #'tkareine/js2-mode-toggle-strict-missing-semi-warning))

(eval-after-load "js2-mode" #'tkareine/js2-mode-customizations)

(defun tkareine/js2-mode-hook ()
  (setq mode-name "JS2")
  (set-fill-column 300))

(add-hook 'js2-mode-hook #'tkareine/js2-mode-hook)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.javascript\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node\\(?:js\\)?" . js2-mode))

;; RJSX: js2-mode with jsx support
(defun tkareine/rjsx-mode-hook ()
  (setq mode-name "RJSX"))

(add-hook 'rjsx-mode-hook #'tkareine/rjsx-mode-hook)

;; ELisp support
(defun tkareine/emacs-lisp-mode-hook ()
  (setq mode-name "ELisp"))

(add-hook 'emacs-lisp-mode-hook #'tkareine/emacs-lisp-mode-hook)

;; Clojure support
(defun tkareine/clojure-mode-customizations ()
  (let ((default-indent 2))
    (define-clojure-indent
      (ANY       default-indent)
      (DELETE    default-indent)
      (GET       default-indent)
      (HEAD      default-indent)
      (OPTIONS   default-indent)
      (PATCH     default-indent)
      (POST      default-indent)
      (PUT       default-indent)
      (context   default-indent)
      (describe  default-indent)
      (it        default-indent)
      (defroutes 'defun))))

(eval-after-load "clojure-mode" #'tkareine/clojure-mode-customizations)

(customize-set-variable 'cider-eval-result-prefix ";; => ")
(customize-set-variable 'cider-repl-result-prefix ";; => ")
(customize-set-variable 'cider-repl-history-file "~/.cider_history")

;; Cider: attempt to use the symbol at point as input for
;; `cider-find-var', and only prompt if that throws an error
(customize-set-variable 'cider-prompt-for-symbol nil)

;; Cider: I want to inject dependencies manually via
;; `~/.lein/profiles.clj'. Otherwise Leiningen's `:pedantic? :abort'
;; setting causes `lein repl' to abort due to overriding version of
;; `org.clojure/tools.nrepl'.
(customize-set-variable 'cider-inject-dependencies-at-jack-in nil)

(custom-set-faces '(cider-result-overlay-face ((t (:background "grey30")))))

(defun tkareine/cider-mode-hook ()
  (local-set-key (kbd "C-c B")       #'cider-connection-browser)
  (local-set-key (kbd "C-c C-v C-b") #'cider-eval-buffer)
  (local-set-key (kbd "C-c M-R")     #'cider-restart))

(add-hook 'cider-mode-hook #'tkareine/cider-mode-hook)
(add-hook 'cider-repl-mode-hook #'tkareine/cider-mode-hook)

;; CoffeeScript support
(customize-set-variable 'coffee-tab-width 2)

;; Haskell support
(customize-set-variable 'haskell-process-suggest-remove-import-lines t)
(customize-set-variable 'haskell-process-auto-import-loaded-modules t)
(customize-set-variable 'haskell-process-log t)
(customize-set-variable 'haskell-process-type 'cabal-repl)

(defun tkareine/haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (turn-on-haskell-decl-scan)
  (interactive-haskell-mode)
  (local-set-key (kbd "<f8>")  #'haskell-navigate-imports)
  (local-set-key (kbd "C-`")   #'haskell-interactive-bring)
  (local-set-key (kbd "C-c c") #'haskell-process-cabal)
  (local-set-key (kbd "C-c o") #'haskell-hoogle))

(add-hook 'haskell-mode-hook #'tkareine/haskell-mode-hook)

;; Sass language support
(customize-set-variable 'scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))

;; Markdown language support
(customize-set-variable 'markdown-command "marked --gfm --tables")
(customize-set-variable 'markdown-hide-urls nil)
(customize-set-variable 'markdown-asymmetric-header t)
(customize-set-variable 'markdown-live-preview-delete-export 'delete-on-export)
(custom-set-faces '(markdown-code-face ((t (:background "#4b4b4b")))))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Ruby file types
(add-to-list 'auto-mode-alist '("/gemfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/guardfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/procfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/rakefile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/vagrantfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("j?ruby\\(?:1.8\\|1.9\\)?" . enh-ruby-mode))

;; Shell script file types
(add-to-list 'auto-mode-alist '("\\.bashrc\\.[a-z0-9-]+\\'" . sh-mode))

;; Textile file types
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; Visual Basic support
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.aspx\\'" . visual-basic-mode))

;; Misc template engines support
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))

;; Interactive regexp builder
(require 're-builder)
(customize-set-variable 'reb-re-syntax 'string)

;; Compilation
(define-key compilation-mode-map (kbd "M-N") #'compilation-next-file)
(define-key compilation-mode-map (kbd "M-P") #'compilation-previous-file)

;; Default major-mode
(customize-set-variable 'major-mode 'text-mode)
