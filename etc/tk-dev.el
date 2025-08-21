;; -*- lexical-binding: t; -*-

;;; Ediff

(use-package ediff-wind
  :custom
  ;; Use current frame for control panel
  (ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; I find it easier to read a diff by having one buffer on the left and
  ;; another on the right
  (ediff-split-window-function 'split-window-horizontally))

;;; Interactive regexp builder

(use-package re-builder
  :custom
  (reb-re-syntax 'string)

  :bind
  (("C-c R" . re-builder)
   :map reb-mode-map
   ("C-c C-k" . reb-quit)
   ("M-n"     . reb-next-match)
   ("M-p"     . reb-prev-match)))

;;; Compilation

(use-package compile
  :bind
  (:map compilation-mode-map
        ("M-N" . compilation-next-file)
        ("M-P" . compilation-previous-file)))

;;; ElDoc

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer nil)

  (eldoc-echo-area-use-multiline-p
   (- max-mini-window-height 0.05)
   "Subtract a bit from the max height in order to avoid vertical \
content clipping (maybe the truncation algorithm has problems \
with bitmaps)")

  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;;; Snippets

(use-package yasnippet
  :ensure t

  :hook
  (lsp-mode . yas-minor-mode-on)

  :commands
  (yas-expand-snippet))

;;; Flycheck

(use-package flycheck
  :ensure t

  :demand

  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc json-python-json))
  (flycheck-temp-prefix ".~flycheck")

  :hook
  (prog-mode)

  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error)))

;;; Tree-sitter

(use-package treesit
  :if (>= emacs-major-version 29)

  :init
  (defvar tk-dev/treesit-language-source-alist
    '((bash       "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.1")
      (css        "https://github.com/tree-sitter/tree-sitter-css" "v0.23.0")
      (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1")
      (json       "https://github.com/tree-sitter/tree-sitter-json" "v0.24.1")
      (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.0")
      (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
      (yaml       "https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
    "Tree-sitter language grammar configuration")

  (defun tk-dev/treesit-install-language-grammars (force-install-all)
    "Install Tree-sitter grammars for configured languages if grammars
are missing. If FORCE-INSTALL-ALL is t, then install grammars for
all configured languages regardless whether they are already
installed. Use FORCE-INSTALL-ALL to update grammars."
    (interactive
     (list (y-or-n-p "Force install all configured Tree-sitter language grammars?")))
    (pcase-dolist (`(,grammar . ,_rest) tk-dev/treesit-language-source-alist)
      ;; Install `grammar' if forced or if we don't have it installed already
      (when (or force-install-all
                (not (treesit-language-available-p grammar)))
        (message "Installing Tree-sitter language grammar %s…" grammar)
        (treesit-install-language-grammar grammar))))

  :custom
  (treesit-max-buffer-size (let ((mb (* 1024 1024))) (* 100 mb)))

  :config
  (dolist (lang tk-dev/treesit-language-source-alist)
    (add-to-list 'treesit-language-source-alist lang))

  (tk-dev/treesit-install-language-grammars nil)

  (dolist (mapping '((sh-mode         . bash-ts-mode)
                     (css-mode        . css-ts-mode)
                     (js-mode         . js-ts-mode)
                     (json-mode       . json-ts-mode)
                     (ruby-mode       . ruby-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (yaml-mode       . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

;;; Magit

(use-package magit
  :ensure t

  :custom
  (magit-define-global-key-bindings nil "Disable default global key bindings because we defined them ourselves")
  (magit-auto-revert-mode nil "Disable `magit-auto-revert-mode' because we're using global-auto-revert-mode")

  :config
  (add-to-list 'tk-looks/minor-mode-alist '(magit-blame-mode magit-blame-mode-lighter))

  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c M-g" .	magit-file-dispatch)))

;; Select VCS backends for VC, the builtin Emacs version control
;; interface
(setq-default vc-handled-backends '(Git))

;;; Xref

(use-package xref
  :bind
  ;; Add additional keybinding, as macOS interprets M-? to show menu bar
  (("C-M-/" . xref-find-references)))

;;; ggtags frontend for GNU global

(use-package ggtags
  :ensure t

  :init
  (defun tk-dev/make-gtags (rootdir)
    "Make gtags files to the current project.

If called with a prefix, specify the directory to make gtags files for."
    (interactive (cl-flet ((read-dir ()
                             (read-directory-name "Make GTAGS to: " nil nil t)))
                   (let ((dir (if current-prefix-arg
                                  (read-dir)
                                (if-let ((proj-dir (projectile-project-root)))
                                    proj-dir
                                  (read-dir)))))
                     (list dir))))
    (let ((current-prefix-arg nil)) ; reset as it might affect future commands
      (ggtags-create-tags rootdir)))

  (defun tk-dev/ggtags-adjust-tag-bounds-for-scss-mode (org-bounds)
    "Adjusts tag bounds so that `$var' gets converted to `var'. The
dollar sign does not belong to SCSS variable symbol in our
configuration for GNU Global."
    (pcase org-bounds
      (`(,org-beg . ,org-end)
       (let* ((tag-str (buffer-substring org-beg org-end))
              (dollar-prefix-length (tk-support/string-prefix-length-with-char ?$ tag-str))
              (new-beg (+ org-beg dollar-prefix-length))
              (new-bounds (if (< new-beg org-end)
                              (cons new-beg org-end)
                            org-bounds)))
         new-bounds))))

  (defun tk-dev/ggtags-bounds-of-tag ()
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (pcase major-mode
        ('scss-mode (tk-dev/ggtags-adjust-tag-bounds-for-scss-mode bounds))
        (- bounds))))

  :custom
  (ggtags-bounds-of-tag-function #'tk-dev/ggtags-bounds-of-tag)

  :config
  ;; Don't change `mode-line-buffer-identification', because we
  ;; show project root dir in the mode line with projectile
  (setq ggtags-mode-line-project-name nil)

  (add-to-list 'tk-looks/minor-mode-alist
               '(ggtags-mode (:eval (if ggtags-navigation-mode " GG[nav]" " GG"))))

  (unbind-key "M-]" ggtags-mode-map)

  :bind
  (("C-c t" . ggtags-mode)
   ("C-c T" . tk-dev/make-gtags)
   :map ggtags-mode-map
   ("C-M-/" . ggtags-find-reference)))

;;; Language Server Protocol
;;;
;;; `https://emacs-lsp.github.io/lsp-mode/'

(use-package lsp-mode
  :ensure t

  :init
  ;; See `(setenv "LSP_USE_PLISTS" "true")' in `early-init.el'
  (setq lsp-use-plists t)

  :custom
  ;; Would like to use `:capf', but `:none' removes a warning message
  ;; about not being able to autoconfigure company-mode
  (lsp-completion-provider :none)

  (lsp-eldoc-render-all t)
  (lsp-enable-text-document-color nil "Rely on Treesitter for font faces")
  (lsp-progress-prefix " … " "Less obtrusive progress status")

  :config
  (add-to-list 'tk-looks/minor-mode-alist
               (assq 'lsp-mode minor-mode-alist))

  :hook
  ((lsp-mode . lsp-enable-which-key-integration))

  :commands
  (lsp lsp-deferred)

  :bind
  (("C-c l" . lsp)
   :map lsp-mode-map
   ("C-c H" . lsp-describe-thing-at-point)
   ("C-M-/" . lsp-find-references)
   ("C-M->" . lsp-find-type-definition)))

(use-package lsp-modeline
  :custom
  (lsp-modeline-code-action-fallback-icon "?" "Less obtrusive code action icon"))

(use-package lsp-ui
  :ensure t

  :custom
  (lsp-ui-doc-delay 0.5 "Number of seconds before showing documentation popup")
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 180)
  (lsp-ui-doc-max-height 40)
  (lsp-ui-doc-include-signature t)

  :commands
  (lsp-ui-mode)

  :bind
  (:map lsp-ui-mode-map
        ("C-c h" . lsp-ui-doc-toggle)
        ("s->"   . lsp-ui-imenu))

  :after
  (lsp-mode))

;;; Apheleia: code formatter runner
;;;
;;; `https://github.com/radian-software/apheleia'

(use-package apheleia
  :ensure t

  :config
  ;; (setq apheleia-log-debug-info t) ; Enable to debug formatters' run commands

  (add-to-list 'tk-looks/minor-mode-alist '(apheleia-mode " Aph"))

  ;; `tk-apheleia-shfmt' wraps `shfmt': if there is an `.editorconfig'
  ;; file in the current working directory or somewhere up in the
  ;; directory tree, use only the `filepath' argument with `shfmt'. This
  ;; makes `shfmt' to respect formatting options from the
  ;; `.editorconfig' file. If the `.editorconfig' file is not found, use
  ;; all the given command line options to execute `shfmt'.
  (add-to-list 'apheleia-formatters '(shfmt . ((tk-init/user-emacs-path "bin/tk-apheleia-shfmt")
                                               filepath
                                               "--language-dialect" (cl-case (bound-and-true-p sh-shell)
                                                                      (sh "posix")
                                                                      (t "bash"))
                                               "--indent" (number-to-string
                                                           (cond
                                                            (indent-tabs-mode 0)
                                                            ((boundp 'sh-basic-offset)
                                                             sh-basic-offset)
                                                            (t 4)))
                                               "--binary-next-line"
                                               "--case-indent")))

  (apheleia-global-mode 1))

;;; CSS and SCSS

(use-package css-mode
  :custom
  (css-indent-offset 2)

  :hook
  ((scss-mode . ggtags-mode)))

;;; C family

(setq-default c-basic-offset 4)

(setq-default c-default-style '((awk-mode  . "awk")
                                (java-mode . "java")
                                (other     . "linux")))

;;; js-mode for `.js' and `.jsx' sources

(use-package js
  :init
  (defun tk-dev/js-lsp-mode-hook ()
    ;; Don't enable LSP in `json-mode' or `jsonc-mode' (which derive
    ;; from `js-mode' via the `javascript-mode' alias)
    (when (not (member major-mode '(json-mode jsonc-mode)))
      (lsp-deferred)))

  :custom
  (js-indent-level 2)

  :config
  ;; `js-base-mode' is the parent mode of both `js-mode' and
  ;; `js-ts-mode'
  (add-hook 'js-base-mode-hook #'tk-dev/js-lsp-mode-hook)

  (unbind-key "M-." js-mode-map)
  (unbind-key "M-." js-ts-mode-map)

  :mode
  (("\\.[cm]?jsx?\\'"  . js-mode)
   ("\\.javascript\\'" . js-mode))

  :interpreter
  (("node\\(?:js\\)?" . js-mode)))

;;; TypeScript for `.ts' and `.tsx' sources

(use-package typescript-mode
  :ensure t

  :custom
  (typescript-indent-level 2)

  :hook
  ((typescript-mode . lsp-deferred))

  :mode
  ("\\.tsx?\\'"))

(use-package typescript-ts-mode
  :hook
  ;; `typescript-ts-base-mode' is the parent mode for both
  ;; `typescript-ts-mode' and `typescript-ts-base-mode'
  ((typescript-ts-base-mode . lsp-deferred)))

;; HTML

(use-package mhtml-mode
  :mode
  ("\\.x?html\\'"))

;;; JSON

(use-package json-mode
  :ensure t

  :mode
  ("\\.json\\'"))

;;; YAML

(use-package yaml-mode
  :ensure t

  :hook
  ((yaml-mode . ggtags-mode))

  :mode
  ("\\.ya?ml\\'"
   "\\.gemrc\\'"))

(use-package yaml-ts-mode
  :hook
  ((yaml-ts-mode . ggtags-mode)))

;;; ELisp

(use-package elisp-mode
  :hook
  (emacs-lisp-mode . smartparens-strict-mode)

  :after
  (smartparens))

(use-package macrostep
  :ensure t

  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)))

;;; Clojure

(use-package clojure-mode
  :config
  (define-clojure-indent
   (ANY       2)
   (DELETE    2)
   (GET       2)
   (HEAD      2)
   (OPTIONS   2)
   (PATCH     2)
   (POST      2)
   (PUT       2)
   (api       'defun)
   (context   1)
   (defroutes 'defun)
   (describe  1)
   (it        1))

  :hook
  (clojure-mode . smartparens-strict-mode)

  :mode
  ("\\.clj\\'"
   "\\.edn\\'")

  :after
  (smartparens))

;;; CIDER

(use-package cider
  :pin melpa-stable

  :init
  (shell-command (mapconcat #'identity
                            `("touch ~/.cider_history"
                              "chmod 600 ~/.cider_history")
                            " && ")
                 t)

  :init
  (defun tk-dev/cider-mode-hook ()
    (local-set-key (kbd "C-c B")   #'cider-connection-browser)
    (local-set-key (kbd "C-c M-l") #'cider-inspect-last-result)
    (local-set-key (kbd "C-c M-R") #'cider-restart))

  :custom
  (cider-eval-result-prefix ";; => ")
  (cider-repl-result-prefix ";; => ")
  (cider-repl-history-file "~/.cider_history")
  (cider-prompt-for-symbol nil "Attempt to use the symbol at point as input for `cider-find-var', and only prompt if that throws an error")
  (cider-inject-dependencies-at-jack-in nil "I want to inject dependencies manually via `~/.lein/profiles.clj'. Otherwise Leiningen's `:pedantic? :abort' setting causes `lein repl' to abort due to overriding version of `org.clojure/tools.nrepl'.")
  (cider-mode-line '(" " (:eval (cider--modeline-info))) "Shorten mode line info")

  :config
  (add-to-list 'tk-looks/minor-mode-alist '(cider-popup-buffer-mode (" cider-tmp")))
  (add-to-list 'tk-looks/minor-mode-alist '(cider-auto-test-mode (cider-mode " Test")))
  (add-to-list 'tk-looks/minor-mode-alist '(cider--debug-mode " DEBUG"))
  (add-to-list 'tk-looks/minor-mode-alist '(cider-mode cider-mode-line))

  (add-hook 'cider-mode-hook #'tk-dev/cider-mode-hook)
  (add-hook 'cider-repl-mode-hook #'tk-dev/cider-mode-hook)

  :custom-face
  (cider-result-overlay-face ((t (:background "grey30"))))

  :after
  (clojure-mode))

;;; Haskell

(use-package haskell-mode
  :init
  (defun tk-dev/haskell-mode-hook ()
    (haskell-indentation-mode)
    (haskell-decl-scan-mode)
    (interactive-haskell-mode))

  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-type 'cabal-repl)

  :config
  (add-hook 'haskell-mode-hook #'tk-dev/haskell-mode-hook)

  :bind
  (:map haskell-mode-map
        ("<f8>"  . haskell-navigate-imports)
        ("C-`"   . haskell-interactive-bring)
        ("C-c c" . haskell-process-cabal)
        ("C-c o" . haskell-hoogle))

  :mode
  ("\\.hs\\'"))

;; Shell scripts

(use-package sh-script
  :hook
  ((sh-mode      . ggtags-mode)
   (bash-ts-mode . ggtags-mode)))

;;; Python

(use-package python
  :hook
  ((python-mode . ggtags-mode)))

;;; Ruby

(use-package ruby-mode
  :hook
  ((ruby-mode    . ggtags-mode)
   (ruby-ts-mode . ggtags-mode))

  :mode
  ("/.Brewfile\\'"
   "/Gemfile-[[:alnum:]]+\\'"))

;;; Rust

(use-package rust-mode
  :ensure t

  :custom
  (rust-format-on-save nil "Use Apheleia instead")
  (rust-rustfmt-switches '())
  (lsp-rust-analyzer-cargo-watch-command "clippy")

  :bind
  (:map rust-mode-map
        ("C-c e"   . lsp-rust-analyzer-expand-macro)
        ("C-x C-e" . lsp-rust-analyzer-run))

  :hook
  ((rust-mode . lsp-deferred))

  :mode
  ("\\.rs\\'"))

;;; Markdown

(use-package markdown-mode
  :ensure t

  :config
  ;; Don't include XHTML DTD for generated HTML, as it affects CSS
  ;; styles
  (advice-add #'markdown-output-standalone-p
              :override
              (lambda () t))

  :custom
  (markdown-command "marked --gfm")
  (markdown-hide-urls nil)
  (markdown-asymmetric-header t)
  (markdown-live-preview-delete-export 'delete-on-export)

  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'"       . gfm-mode)))

;;; Dockerfile

(use-package dockerfile-ts-mode
  :mode
  ("/Dockerfile\\(?:\\.[^/]+\\)?\\'"))

;;; Configuration files

(add-to-list 'auto-mode-alist '("/\\.aws/config\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/\\.aws/credentials\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/\\.s3cfg\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/Cargo\\.lock\\'" . conf-toml-mode))
