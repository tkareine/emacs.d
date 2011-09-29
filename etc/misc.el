;; Prefer UTF-8 encoding for input and output
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; IDO: Flexible matching (try exact match first, then fuzzy)
(setq ido-enable-flex-matching t)

;; Hard wrapping at column number
(set-fill-column 72)

;; Default indentation
(setq standard-indent 2)

;; Markdown file types
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
