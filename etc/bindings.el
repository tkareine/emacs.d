;; OS X: allow entering special chars via Option key
(setq mac-option-modifier nil)

;; OS X: modifier keys
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'super)

;; Custom key bindings
(global-set-key (kbd "C-+")            'text-scale-increase)
(global-set-key (kbd "C--")            'text-scale-decrease)
(global-set-key (kbd "C-B")            'tkareine/backward-char-5)
(global-set-key (kbd "C-F")            'tkareine/forward-char-5)
(global-set-key (kbd "C-M-r")          'isearch-backward)
(global-set-key (kbd "C-M-s")          'isearch-forward)
(global-set-key (kbd "C-N")            'tkareine/next-line-5)
(global-set-key (kbd "C-P")            'tkareine/previous-line-5)
(global-set-key (kbd "C-c ?")          'dash-at-point)
(global-set-key (kbd "C-c A")          'ag)
(global-set-key (kbd "C-c B")          'browse-url-at-point)
(global-set-key (kbd "C-c C")          'comment-dwim)
(global-set-key (kbd "C-c C-SPC")      'helm-all-mark-rings)
(global-set-key (kbd "C-c C-a")        'ag-regexp)
(global-set-key (kbd "C-c C-c M-x")    'execute-extended-command)
(global-set-key (kbd "C-c C-s")        'helm-do-ag)
(global-set-key (kbd "C-c P")          'tkareine/file-path-to-clipboard)
(global-set-key (kbd "C-c S")          'helm-ag-this-file)
(global-set-key (kbd "C-c SPC")        'helm-semantic-or-imenu)
(global-set-key (kbd "C-c a")          'ag-project-regexp)
(global-set-key (kbd "C-c b")          'helm-resume)
(global-set-key (kbd "C-c c")          'tkareine/comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c g")          'magit-status)
(global-set-key (kbd "C-c h o")        'helm-occur)
(global-set-key (kbd "C-c h x")        'helm-register)
(global-set-key (kbd "C-c h")          'helm-command-prefix)
(global-set-key (kbd "C-c j")          'tkareine/js2-mode-toggle-strict-missing-semi-warning)
(global-set-key (kbd "C-c s")          'helm-projectile-ag)
(global-set-key (kbd "C-c t")          'delete-trailing-whitespace)
(global-set-key (kbd "C-c w")          'whitespace-mode)
(global-set-key (kbd "C-r")            'isearch-backward-regexp)
(global-set-key (kbd "C-s")            'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b")        'ibuffer)
(global-set-key (kbd "C-x C-f")        'helm-find-files)
(global-set-key (kbd "C-x b")          'helm-mini)
(global-set-key (kbd "C-ä")            'insert-register)
(global-set-key (kbd "C-ö")            'copy-to-register)
(global-set-key (kbd "M-<kp-delete>")  'kill-word)
(global-set-key (kbd "M-J")            'delete-indentation)
(global-set-key (kbd "M-SPC")          'fixup-whitespace)
(global-set-key (kbd "M-j")            'tkareine/join-line)
(global-set-key (kbd "M-x")            'helm-M-x)
(global-set-key (kbd "M-y")            'helm-show-kill-ring)
(global-set-key (kbd "M-§")            'other-frame)
(global-set-key (kbd "M-ä")            'point-to-register)
(global-set-key (kbd "M-å")            'er/expand-region)
(global-set-key (kbd "M-ö")            'jump-to-register)
(global-set-key (kbd "S-<down>")       'windmove-down)
(global-set-key (kbd "S-<left>")       'windmove-left)
(global-set-key (kbd "S-<right>")      'windmove-right)
(global-set-key (kbd "S-<up>")         'windmove-up)
(global-set-key (kbd "s-F")            'find-file-at-point)
(global-set-key (kbd "s-SPC")          'hippie-expand)
(global-set-key (kbd "s-d")            'helm-projectile-find-dir)
(global-set-key (kbd "s-f")            'helm-projectile-find-file)
(global-set-key (kbd "s-g")            'helm-projectile-find-file-dwim)
(global-set-key (kbd "s-v")            'helm-projectile-find-other-file)

;; Auto complete: advanced completion
;; <http://cx4a.org/software/auto-complete/manual.html#auto-complete_command>
(define-key ac-mode-map (kbd "C-u TAB") 'auto-complete)

;; Helm: switch bindings for TAB andn C-z
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; Helm: make TAB work in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

;; Just use C-<left|right> to move within subwords
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))

;; In paredit-mode, allow using C-<left|right> to move within subwords
;; and use M-<left|right> as replacement
(add-hook 'paredit-mode-hook
          (lambda ()
            (let ((oldmap (cdr (assoc 'paredit-mode minor-mode-map-alist)))
                  (newmap (make-sparse-keymap)))
              (set-keymap-parent newmap oldmap)
              (define-key newmap (kbd "C-<left>") nil)
              (define-key newmap (kbd "C-<right>") nil)
              (define-key newmap (kbd "M-<left>") 'paredit-forward-barf-sexp)
              (define-key newmap (kbd "M-<right>") 'paredit-forward-slurp-sexp)
              (make-local-variable 'minor-mode-overriding-map-alist)
              (push `(paredit-mode . ,newmap) minor-mode-overriding-map-alist))))
