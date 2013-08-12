;; OS X: allow entering special chars via Option key
(setq mac-option-modifier nil)

;; OS X: modifier keys
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'super)

;; Custom key bindings
(global-set-key (kbd "C-B")    'my-backward-char-5)
(global-set-key (kbd "C-F")    'my-forward-char-5)
(global-set-key (kbd "C-N")    'my-next-line-5)
(global-set-key (kbd "C-P")    'my-previous-line-5)
(global-set-key (kbd "C-c a")  'ag-project-at-point)
(global-set-key (kbd "C-c c")  'comment-or-uncomment-region)
(global-set-key (kbd "C-c j")  'my-js2-mode-toggle-strict-missing-semi-warning)
(global-set-key (kbd "C-c p")  'my-file-path-to-clipboard)
(global-set-key (kbd "C-c s")  'ag-regexp-project-at-point)
(global-set-key (kbd "C-c t")  'delete-trailing-whitespace)
(global-set-key (kbd "C-c w")  'whitespace-mode)
(global-set-key (kbd "M-J")    'delete-indentation)
(global-set-key (kbd "M-j")    'my-join-line)
(global-set-key (kbd "M-å")    'er/expand-region)
(global-set-key (kbd "M-ö")    'fixup-whitespace)
(global-set-key (kbd "s-SPC")  'hippie-expand)
(global-set-key [M-kp-delete]  'kill-word)
(global-set-key [kp-delete]    'delete-char)

;; Auto complete: advanced completion
;; <http://cx4a.org/software/auto-complete/manual.html#auto-complete_command>
(define-key ac-mode-map (kbd "C-u TAB") 'auto-complete)
