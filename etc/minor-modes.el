;; Typing text replaces active selection
(delete-selection-mode t)

;; Enable auto pairing of brackets and quotation marks
(electric-pair-mode 1)

;; Dired: allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Tramp: Prefer ssh
(setq tramp-default-method "ssh")

;; DiredX: for Dired Jump
(require 'dired-x)

;; IDO: Enable globally
(ido-mode t)

;; IDO: Flexible matching (try exact match first, then fuzzy)
(setq ido-enable-flex-matching t)

;; CUA: Enable globally for enhanced rectangle support
(cua-mode t)

;; CUA: Disable binding of C-x, C-c, C-v
(setq cua-enable-cua-keys nil)

;; Uniquify: append dir name to buffers with similar filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Saveplace: save point location in the buffer when revisiting the buffer
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (my-dotfile-path "places"))

;; Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (my-dotfile-path "ac-dictionary"))
(ac-config-default)

;; Enable UndoTree globally
(global-undo-tree-mode)

;; Ag: enable search highlighting
(setq ag-highlight-search t)

;; Projectile: file finder, with Grizzl completion system
(require 'grizzl)
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;; Paredit
(add-hook 'clojure-mode-hook                     'paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'ielm-mode-hook                        'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            'paredit-mode)
(add-hook 'lisp-mode-hook                        'paredit-mode)
(add-hook 'scheme-mode-hook                      'paredit-mode)
