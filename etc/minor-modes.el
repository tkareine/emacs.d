;; Typing text replaces active selection
(delete-selection-mode t)

;; Enable auto pairing of brackets and quotation marks
(electric-pair-mode 1)

;; Dired: allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Tramp: prefer ssh
(setq tramp-default-method "ssh")

;; DiredX: for Dired Jump
(require 'dired-x)

;; IDO: enable globally
(ido-mode t)

;; IDO: flexible matching (try exact match first, then fuzzy)
(setq ido-enable-flex-matching t)

;; IDO vertical mode: Enable globally
(ido-vertical-mode t)

;; IDO vertical mode: up/down to navigate, left/right to browse history
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; CUA: enable globally for enhanced rectangle support
(cua-mode t)

;; CUA: disable binding of C-x, C-c, C-v
(setq cua-enable-cua-keys nil)

;; Uniquify: append dir name to buffers with similar filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Saveplace: save point location in the buffer when revisiting the buffer
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (tkareine/dotfile-path "places"))

;; Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (tkareine/dotfile-path "ac-dictionary"))
(ac-config-default)

;; Enable UndoTree globally
(global-undo-tree-mode)

;; Ag: enable search highlighting
(setq ag-highlight-search t)

;; Projectile: file finder, with Grizzl completion system
(projectile-global-mode)
(setq projectile-completion-system 'ido)

;; GitGutter
(global-git-gutter-mode t)
(setq git-gutter:lighter " GG")

;; Paredit
(add-hook 'clojure-mode-hook                     'paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'ielm-mode-hook                        'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            'paredit-mode)
(add-hook 'lisp-mode-hook                        'paredit-mode)
(add-hook 'scheme-mode-hook                      'paredit-mode)

(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (tkareine/active-region-or-line)))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive (tkareine/active-region-or-line)))
