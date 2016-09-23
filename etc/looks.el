;; Disable visible bell, is broken on OS X El Capitan
(customize-set-variable 'visible-bell nil)

;; Replace ring-bell with my own function
(customize-set-variable 'ring-bell-function #'tkareine/visible-bell)

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

;; Show buffer size in the mode line
(size-indication-mode 1)

;; No blinking cursor
(blink-cursor-mode 0)

;; Highlight long lines when whitespace-mode is enabled
(setq whitespace-line-column 140)

;; Highlight trailing whitespaces in lines
(customize-set-variable 'show-trailing-whitespace t)

;; Show file size
(size-indication-mode t)

;; Font
;; (set-face-font 'default "Inconsolata-16")
(set-face-font 'default "Input-14")
(customize-set-variable 'line-spacing 2)

;; Frame width and height
(if (and (boundp 'window-system) window-system) (set-frame-size (selected-frame) 140 60))

;; Color theme
;;(require 'color-theme-my-twilight)
;;(color-theme-my-twilight)
(load-theme 'zenburn t)
