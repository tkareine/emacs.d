;; OS X: allow entering special chars via Option key
(setq mac-option-modifier nil)

;; OS X: use Command as meta key
(setq mac-command-modifier 'meta)

;; Custom key bindings
(global-set-key [kp-delete]    'delete-char)
(global-set-key [M-kp-delete]  'kill-word)
(global-set-key (kbd "C-c c")  'comment-or-uncomment-region)
(global-set-key (kbd "C-c p")  'my-file-path-to-clipboard)
(global-set-key (kbd "C-c w")  'whitespace-mode)
(global-set-key (kbd "M-+")    'hippie-expand)
(global-set-key (kbd "M-ö")    'fixup-whitespace)

;; Prevent extraneous tabs
(setq-default indent-tabs-mode nil)
