;; Syntax higlighting where applicable
(global-font-lock-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Standard selection-highlighting behavior of other edit
(transient-mark-mode t)

;; See matching pairs of parentheses and other characters
(show-paren-mode t)

;; Show current line and column in the mode line
(line-number-mode t)
(column-number-mode t)

;; No blinking cursor
(blink-cursor-mode 0)

;; Whitespace highlighting options
(setq whitespace-line-column 140)

;; Show file size
(size-indication-mode t)

;; Font
(set-default-font "Inconsolata-14")
(setq-default line-spacing 1)

;; Frame width and height
(if (and (boundp 'window-system) window-system) (set-frame-size (selected-frame) 140 60))

;; Color theme
;;(require 'color-theme-my-twilight)
;;(color-theme-my-twilight)
(load-theme 'zenburn t)
