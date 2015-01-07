;; Typing text replaces active selection
(delete-selection-mode t)

;; Enable auto pairing of brackets and quotation marks
(electric-pair-mode 1)

;; Dired: allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Revert file buffer if changed externally
(global-auto-revert-mode t)

;; Default major-mode
(setq major-mode 'text-mode)

;; Add missing newline to file automatically when saving
(setq require-final-newline t)

;; Tramp: prefer ssh
(setq tramp-default-method "ssh")

;; DiredX: for Dired Jump
(require 'dired-x)

;; CUA: enable globally for enhanced rectangle support
(cua-selection-mode t)

;; Which function: enable globally
(which-function-mode t)

;; Uniquify: append dir name to buffers with similar filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Saveplace: save point location in the buffer when revisiting the buffer
(setq-default save-place t)
(require 'saveplace)
(setq history-delete-duplicates t)
(setq save-place-file (tkareine/dotfile-path "saveplace"))
(setq savehist-file (tkareine/dotfile-path "savehist"))
(savehist-mode 1)

;; Recentf: store save file under emacs conf dir
(setq recentf-save-file (tkareine/dotfile-path "recentf"))

;; Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (tkareine/dotfile-path "ac-dictionary"))
(ac-config-default)

;; Helm
(require 'helm-config)
;; Helm: always show helm buffers below
(setq helm-split-window-default-side 'below)
(setq helm-always-two-windows t)
;; Helm: enable globally
(helm-mode t)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)
(add-to-list 'projectile-other-file-alist '("html" "js" "css"))
(add-to-list 'projectile-other-file-alist '("js" "html" "css"))
(add-to-list 'projectile-other-file-alist '("css" "html" "js"))

;; Enable UndoTree globally
(global-undo-tree-mode)

;; Ag: enable search highlighting
(setq ag-highlight-search t)

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
