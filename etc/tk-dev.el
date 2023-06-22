;; -*- lexical-binding: t; -*-

;;; Ediff

;; Use current frame for control panel
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; I find it easier to read a diff by having one buffer on the left and
;; another on the right
(setq-default ediff-split-window-function 'split-window-horizontally)

;;; Interactive regexp builder

(bind-key "C-c R" #'re-builder)

(setq-default reb-re-syntax 'string)

(with-eval-after-load 're-builder
  (bind-keys :map reb-mode-map
             ("C-c C-k" . reb-quit)
             ("M-n"     . reb-next-match)
             ("M-p"     . reb-prev-match)))

;;; Compilation

(with-eval-after-load 'compile
  (bind-keys :map compilation-mode-map
             ("M-N" . compilation-next-file)
             ("M-P" . compilation-previous-file)))

;;; Company

(use-package company
  :ensure t

  :demand

  :config
  (global-company-mode 1)

  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 2 "The minimum prefix length before showing idle completion")
  (company-tooltip-align-annotations t "Align annotations to the right tooltip border")
  (company-dabbrev-downcase nil "Don't lowercase completion candidates (dabbrev backend)")
  (company-dabbrev-ignore-case t "Ignore case when collecting completion candidates and copy candidate verbatim")
  (company-dabbrev-code-ignore-case t "Ignore case when collecting completion candidates and copy candidate verbatim")
  (company-backends '(company-nxml
                      company-css
                      company-semantic
                      company-clang
                      (company-capf company-dabbrev-code)
                      company-files
                      company-keywords
                      company-dabbrev)
                    "Use relevant completion engines only. Especially, put `company-capf' and `company-dabbrev-code' into same group so that the latter adds candidates the former misses.")

  :bind
  (("C-<tab>" . company-complete)))

;;; Snippets

(use-package yasnippet
  :ensure t

  :commands
  (yas-expand-snippet)

  :hook
  (lsp-mode . yas-minor-mode-on))

;;; Flycheck

(use-package flycheck
  :ensure t

  :demand

  :config
  (global-flycheck-mode 1)

  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc json-python-json))
  (flycheck-temp-prefix ".~flycheck"))

;;; Tree-sitter for Emacs before v29
;;;
;;; Docs: https://emacs-tree-sitter.github.io/

(use-package tree-sitter
  :ensure t

  :config
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t

  :after tree-sitter)

;;; Magit

(use-package magit
  :ensure t

  :custom
  (magit-completing-read-function #'ivy-completing-read)
  (magit-auto-revert-mode nil "Disable `magit-auto-revert-mode', because we're using global-auto-revert-mode")

  :custom-face
  (magit-diff-context ((t (:background "grey25"))))
  (magit-diff-context-highlight ((t (:background "grey32"))))
  (magit-diff-hunk-heading ((t (:background "#3e5f76"))))
  (magit-diff-hunk-heading-highlight ((t (:background "#619abf"))))

  :bind
  (("C-x g" . magit-status)))

;; Disable Emacs' Version Control interface
;; (setq-default vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg Mtn))
(setq-default vc-handled-backends nil)

;;; Xref

;; Add additional keybinding, as macOS interprets M-? to show menu bar
(bind-key "C-M-/" #'xref-find-references)

;;; ggtags frontend for GNU global

(use-package ggtags
  :ensure t

  :config
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

  ;; don't change `mode-line-buffer-identification', because we
  ;; show project root dir in the mode line with projectile
  (setq ggtags-mode-line-project-name nil)

  (add-to-list 'tk-looks/minor-mode-alist
               '(ggtags-mode (:eval (if ggtags-navigation-mode " GG[nav]" " GG"))))

  (bind-keys :map ggtags-mode-map
             ("M-]"   . nil)
             ("C-M-/" . ggtags-find-reference))

  :custom
  (ggtags-bounds-of-tag-function #'tk-dev/ggtags-bounds-of-tag)

  :bind
  (("C-c T" . tk-dev/make-gtags)
   :map ggtags-mode-map
   ("C-c t" . ggtags-find-tag-dwim))

  :hook
  (enh-ruby-mode
   less-css-mode
   python-mode
   scss-mode
   sh-mode
   yaml-mode))

;;; LSP

(use-package lsp-mode
  :ensure t

  :commands
  (lsp lsp-deferred)

  :config
  (add-to-list 'tk-looks/minor-mode-alist
               (assq 'lsp-mode minor-mode-alist))

  :hook
  ((lsp-mode        . lsp-enable-which-key-integration)
   (rust-mode       . lsp)
   ;; don't add hook to rjsx-mode-hook, because rjsx-mode derives from js2-mode
   (js2-mode        . lsp)
   (typescript-mode . lsp))

  :bind
  (("C-c l" . lsp)
   :map lsp-mode-map
   ("C-c H" . lsp-describe-thing-at-point)
   ("C-M-/" . lsp-find-references)
   ("C-M->" . lsp-find-type-definition)))

(use-package lsp-ui
  :ensure t

  :commands
  (lsp-ui-mode)

  :custom
  (lsp-ui-doc-delay 0.5 "Number of seconds before showing documentation popup")
  (lsp-ui-doc-position 'top)

  :bind
  (:map lsp-ui-mode-map
   ("C-c h" . lsp-ui-doc-glance)))

(use-package lsp-ivy
  :commands
  (lsp-ivy-workspace-symbol))

;;; CSS

(setq-default css-indent-offset 2)

;;; C family

(setq-default c-basic-offset 4)

(setq-default c-default-style '((awk-mode  . "awk")
                                (java-mode . "java")
                                (other     . "linux")))

;;; Prettier: format buffer with `prettier' upon save automatically

(use-package prettier
  :ensure t

  :commands
  (prettier-mode)

  :config
  (add-to-list 'tk-looks/minor-mode-alist '(prettier-mode (" Prettier")) t)

  :hook
  (html-mode
   json-mode
   js2-mode
   ;; don't add hook to rjsx-mode-hook, because rjsx-mode derives from js2-mode
   typescript-mode
   yaml-mode))

;;; js2-mode for `.js' sources

(setq-default js-indent-level 2)

(use-package js2-mode
  :ensure t

  :config
  ;; Don't double-indent multiline statement
  (advice-add #'js--multi-line-declaration-indentation
              :override
              #'ignore)

  (defun tk-dev/js2-mode-trigger-strict-warning-p (msg-id &rest _args)
    (not (member msg-id '("msg.no.side.effects"))))

  ;; Filter out selected warnings
  (advice-add #'js2-add-strict-warning
              :before-while
              #'tk-dev/js2-mode-trigger-strict-warning-p)

  (defun tk-dev/js2-mode-toggle-strict-missing-semi-warning ()
    (interactive)
    (setq js2-strict-missing-semi-warning (eq js2-strict-missing-semi-warning nil))
    (js2-mode 1))

  (put 'js2-include-node-externs 'safe-local-variable 'booleanp)

  (defun tk-dev/js2-mode-reparse-current-buffer ()
    (js2-mode-idle-reparse (current-buffer)))

  (defun tk-dev/js2-mode-hook ()
    (setq mode-name "JS2")
    (add-hook 'after-revert-hook #'tk-dev/js2-mode-reparse-current-buffer nil t))

  (add-hook 'js2-mode-hook #'tk-dev/js2-mode-hook)

  (bind-keys :map js2-mode-map
             ("M-." . nil)
             ("C-c j" . tk-dev/js2-mode-toggle-strict-missing-semi-warning))

  :custom
  (js2-basic-offset 2)
  (js2-bounce-indent-p t)
  (js2-concat-multiline-strings nil)
  (js2-highlight-level 3)
  (js2-missing-semi-one-line-override t)
  (js2-strict-missing-semi-warning nil)

  :custom-face
  (js2-private-member ((t (:foreground "coral1"))))

  :mode
  ("\\.[cm]?js\\'"
   "\\.javascript\\'")

  :interpreter
  ("node\\(?:js\\)?"))

;;; RJSX: js2-mode for `.jsx' sources

(use-package rjsx-mode
  :ensure t

  :config
  (defun tk-dev/rjsx-mode-hook ()
    (setq mode-name "RJSX")
    (add-hook 'after-revert-hook #'tk-dev/js2-mode-reparse-current-buffer nil t))

  (add-hook 'rjsx-mode-hook #'tk-dev/rjsx-mode-hook)

  :mode
  ("\\.jsx\\'"))

;;; TypeScript for `.ts' and `.tsx' sources

(use-package typescript-mode
  :ensure t

  :custom
  (typescript-indent-level 2)

  :mode
  ("\\.tsx?\\'"))

;;; JSON

(use-package json-mode
  :ensure t

  :mode
  ("\\.json\\'"))

;;; YAML

(use-package yaml-mode
  :ensure t

  :mode
  ("/\\.ya?ml\\'"
   "/\\.gemrc\\'"))

;;; ELisp

(defun tk-dev/emacs-lisp-mode-hook ()
  (setq mode-name "ELisp")
  (smartparens-strict-mode 1))

(add-hook 'emacs-lisp-mode-hook #'tk-dev/emacs-lisp-mode-hook)

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

  (defun tk-dev/clojure-mode-hook ()
    (smartparens-strict-mode 1))

  (add-hook 'clojure-mode-hook #'tk-dev/clojure-mode-hook)

  :mode
  ("/\\.clj\\'"
   "/\\.edn\\'"))

;;; CIDER

(use-package cider
  :pin melpa-stable

  :init
  (shell-command (mapconcat #'identity
                            `("touch ~/.cider_history"
                              "chmod 600 ~/.cider_history")
                            " && ")
                 t)

  :config
  (add-to-list 'tk-looks/minor-mode-alist '(cider-popup-buffer-mode (" cider-tmp")))
  (add-to-list 'tk-looks/minor-mode-alist '(cider-auto-test-mode (cider-mode " Test")))
  (add-to-list 'tk-looks/minor-mode-alist '(cider--debug-mode " DEBUG"))
  (add-to-list 'tk-looks/minor-mode-alist '(cider-mode cider-mode-line))

  (defun tk-dev/cider-mode-hook ()
    (local-set-key (kbd "C-c B")   #'cider-connection-browser)
    (local-set-key (kbd "C-c M-l") #'cider-inspect-last-result)
    (local-set-key (kbd "C-c M-R") #'cider-restart))

  (add-hook 'cider-mode-hook #'tk-dev/cider-mode-hook)
  (add-hook 'cider-repl-mode-hook #'tk-dev/cider-mode-hook)

  :custom
  (cider-eval-result-prefix ";; => ")
  (cider-repl-result-prefix ";; => ")
  (cider-repl-history-file "~/.cider_history")
  (cider-prompt-for-symbol nil "Attempt to use the symbol at point as input for `cider-find-var', and only prompt if that throws an error")
  (cider-inject-dependencies-at-jack-in nil "I want to inject dependencies manually via `~/.lein/profiles.clj'. Otherwise Leiningen's `:pedantic? :abort' setting causes `lein repl' to abort due to overriding version of `org.clojure/tools.nrepl'.")
  (cider-mode-line '(" " (:eval (cider--modeline-info))) "Shorten mode line info")

  :custom-face
  (cider-result-overlay-face ((t (:background "grey30"))))

  :after
  (clojure-mode))

;;; Haskell

(use-package haskell-mode
  :config
  (defun tk-dev/haskell-mode-hook ()
    (haskell-indentation-mode)
    (haskell-decl-scan-mode)
    (interactive-haskell-mode))

  (add-hook 'haskell-mode-hook #'tk-dev/haskell-mode-hook)

  (bind-keys :map haskell-mode-map
             ("<f8>"  . haskell-navigate-imports)
             ("C-`"   . haskell-interactive-bring)
             ("C-c c" . haskell-process-cabal)
             ("C-c o" . haskell-hoogle))

  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-type 'cabal-repl)

  :mode
  ("/\\.hs\\'"))

;;; Rust

(use-package rust-mode
  :custom
  (rust-format-on-save t)
  (rust-rustfmt-switches '())

  :bind
  (:map rust-mode-map
        ("C-c e"   . lsp-rust-analyzer-expand-macro)
        ("C-x C-e" . lsp-rust-analyzer-run))

  :mode
  ("/\\.rs\\'"))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  :after
  (rust-mode))

;;; Markdown

(use-package markdown-mode
  :ensure t

  :config
  ;; don't include xhtml dtd for generated html, as it affects CSS
  ;; styles
  (advice-add #'markdown-output-standalone-p
              :override
              (lambda () t))

  :custom
  (markdown-command "marked --gfm")
  (markdown-hide-urls nil)
  (markdown-asymmetric-header t)
  (markdown-live-preview-delete-export 'delete-on-export)

  :custom-face
  (markdown-code-face ((t (:background "#4b4b4b"))))

  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'"       . gfm-mode)))

;;; Ruby

(use-package enh-ruby-mode
  :mode
  ("/gemfile\\'"
   "/guardfile\\'"
   "/rakefile\\'"
   "/vagrantfile\\'"
   "\\.rake\\'"
   "\\.rb\\'")

  :interpreter
  ("j?ruby\\(?:1.8\\|1.9\\)?"))

;;; Configuration files

(add-to-list 'auto-mode-alist '("/\\.aws/config\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/\\.aws/credentials\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/\\.s3cfg\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/Cargo\\.lock\\'" . conf-toml-mode))

;;; restclient

(use-package restclient
  :ensure t

  :defer t)
