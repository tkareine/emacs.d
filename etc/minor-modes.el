;; Typing text replaces active selection
(delete-selection-mode t)

;; Enable auto pairing of brackets and quotation marks
(electric-pair-mode 1)

;; Revert file buffer if changed externally
(global-auto-revert-mode t)

;; Tramp: prefer ssh
(customize-set-variable 'tramp-default-method "ssh")

;; Dired: allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; DiredX: for Dired Jump
(require 'dired-x)

;; CUA: enable globally for enhanced rectangle support
(cua-selection-mode t)

;; Which function: enable globally
(which-function-mode t)

;; Uniquify: append dir name to buffers with similar filenames
(require 'uniquify)
(customize-set-variable 'uniquify-buffer-name-style 'forward)

;; Saveplace: save point location in the buffer when revisiting the buffer
(require 'saveplace)
(customize-set-variable 'save-place t)
(customize-set-variable 'history-delete-duplicates t)
(customize-set-variable 'save-place-file (tkareine/dotfile-path "saveplace"))
(customize-set-variable 'savehist-file (tkareine/dotfile-path "savehist"))
(savehist-mode 1)

;; Recentf: store save file under emacs conf dir
(customize-set-variable 'recentf-save-file (tkareine/dotfile-path "recentf"))

;; Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (tkareine/dotfile-path "ac-dictionary"))
(ac-config-default)
;; Auto complete: advanced completion
;; <http://cx4a.org/software/auto-complete/manual.html#auto-complete_command>
(define-key ac-mode-map (kbd "C-u TAB") #'auto-complete)

;; Helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c C-SPC")      #'helm-all-mark-rings)
(global-set-key (kbd "C-c C-s")        #'helm-do-ag)
(global-set-key (kbd "C-c S")          #'helm-ag-this-file)
(global-set-key (kbd "C-c SPC")        #'helm-semantic-or-imenu)
(global-set-key (kbd "C-c b")          #'helm-resume)
(global-set-key (kbd "C-c h o")        #'helm-occur)
(global-set-key (kbd "C-c h x")        #'helm-register)
(global-set-key (kbd "C-c h")          #'helm-command-prefix)
(global-set-key (kbd "C-x C-f")        #'helm-find-files)
(global-set-key (kbd "C-x b")          #'helm-mini)
(global-set-key (kbd "M-x")            #'helm-M-x)
(global-set-key (kbd "M-y")            #'helm-show-kill-ring)
;; Helm: switch bindings for TAB andn C-z
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)
;; Helm: make TAB work in terminal
(define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
;; Helm: enter directory automatically when only one candidate matches
;; input
(customize-set-variable 'helm-ff-auto-update-initial-value t)
;; Helm: always show helm buffers below
(customize-set-variable 'helm-split-window-default-side 'below)
(customize-set-variable 'helm-always-two-windows t)
(customize-set-variable 'helm-locate-command
      (pcase system-type
        (`gnu/linux "locate %s -e -A %s")
        (`darwin "mdfind -name %s %s")
        (`windows-nt "es %s %s")
        (_ "locate %s %s")))
;; Helm: enable
(helm-mode t)

;; Projectile
(require 'helm-projectile)
(global-set-key (kbd "C-c d")          #'helm-projectile-find-dir)
(global-set-key (kbd "C-c f")          #'helm-projectile-find-file)
(global-set-key (kbd "C-c g")          #'helm-projectile-find-file-dwim)
(global-set-key (kbd "C-c s")          #'helm-projectile-ag)
(global-set-key (kbd "C-c v")          #'helm-projectile-find-other-file)
(customize-set-variable 'projectile-completion-system 'helm)
(customize-set-variable 'projectile-switch-project-action 'helm-projectile)
(add-to-list 'projectile-other-file-alist '("html" "js" "css"))
(add-to-list 'projectile-other-file-alist '("js" "html" "css"))
(add-to-list 'projectile-other-file-alist '("css" "html" "js"))
(projectile-global-mode)
(helm-projectile-on)

;; UndoTree
(global-undo-tree-mode)

;; Ag
;; Ag: enable search highlighting
(setq ag-highlight-search t)
(global-set-key (kbd "C-c A")          #'ag)
(global-set-key (kbd "C-c a")          #'ag-project-regexp)
(global-set-key (kbd "C-c C-a")        #'ag-regexp)

;; GitGutter
(global-git-gutter-mode t)
(customize-set-variable 'git-gutter:lighter " GG")

;; Magit
(global-set-key (kbd "C-x g")          #'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Paredit
;; Paredit: just use C-<left|right> to move within subwords
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))
;; Paredit: allow using C-<left|right> to move within subwords and use
;; M-<left|right> as replacement
(add-hook 'paredit-mode-hook
          (lambda ()
            (let ((oldmap (cdr (assoc 'paredit-mode minor-mode-map-alist)))
                  (newmap (make-sparse-keymap)))
              (set-keymap-parent newmap oldmap)
              (define-key newmap (kbd "C-<left>") nil)
              (define-key newmap (kbd "C-<right>") nil)
              (define-key newmap (kbd "M-<left>") #'paredit-forward-barf-sexp)
              (define-key newmap (kbd "M-<right>") #'paredit-forward-slurp-sexp)
              (make-local-variable 'minor-mode-overriding-map-alist)
              (push `(paredit-mode . ,newmap) minor-mode-overriding-map-alist))))
(add-hook 'clojure-mode-hook                     #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'ielm-mode-hook                        #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'paredit-mode)
(add-hook 'lisp-mode-hook                        #'paredit-mode)
(add-hook 'scheme-mode-hook                      #'paredit-mode)

;; Dash
(if (eq system-type 'darwin)
    (global-set-key (kbd "C-c ?")      #'dash-at-point))

;; Expand-reqion
(global-set-key (kbd "C-=")            #'er/expand-region)

;; Flycheck: enable globally
(add-hook 'after-init-hook #'global-flycheck-mode)
