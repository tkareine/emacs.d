;; IDO: Flexible matching (try exact match first, then fuzzy)
(setq ido-enable-flex-matching t)

;; Hard wrapping at column number
(set-fill-column 72)

;; Default indentation
(setq standard-indent 2)

;; Markdown file types
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
