;; OS X: allow entering special chars via Option key
(setq mac-option-modifier nil)

;; OS X: modifier keys
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'super)

;; Custom key bindings
(global-set-key [kp-delete]    'delete-char)
(global-set-key [M-kp-delete]  'kill-word)
(global-set-key (kbd "C-c c")  'comment-or-uncomment-region)
(global-set-key (kbd "C-c p")  'my-file-path-to-clipboard)
(global-set-key (kbd "C-c t")  'delete-trailing-whitespace)
(global-set-key (kbd "C-c w")  'whitespace-mode)
(global-set-key (kbd "s-SPC")  'hippie-expand)
(global-set-key (kbd "M-j")    'my-join-line)
(global-set-key (kbd "M-å")    'er/expand-region)
(global-set-key (kbd "M-ö")    'fixup-whitespace)

;; Auto complete: advanced completion
;; <http://cx4a.org/software/auto-complete/manual.html#auto-complete_command>
(define-key ac-mode-map (kbd "C-u TAB") 'auto-complete)

;; Prevent extraneous tabs
(setq-default indent-tabs-mode nil)
