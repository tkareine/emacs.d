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

;; Recentf: for showing list of recently opened files
(require 'recentf)
;; Recentf: store save file under emacs conf dir
(customize-set-variable 'recentf-save-file (tkareine/dotfile-path "recentf"))
;; Recentf: exclude the recentf save file and Emacs ELPA autoloads
(customize-set-variable 'recentf-exclude (list
                                          (concat "\\`" (expand-file-name (tkareine/dotfile-path "recentf")) "\\'")
                                          (concat "\\`" (expand-file-name (tkareine/dotfile-path "elpa")) "/.*-autoloads.elc?\\'")))
;; Recentf: save the list of recent files periodically. Normally,
;; Recentf saves the list when Emacs exits cleanly. If Emacs crashes,
;; that save is probably not done.
(run-at-time (* 5 60) (* 5 60) (lambda ()
                                 (let ((inhibit-message t))
                                   (recentf-save-list))))
;; Recentf: enable globally
(recentf-mode)

;; Hippie-expand
(global-set-key (kbd "s-SPC") #'hippie-expand)

;; xref: tags
(defun tkareine/make-tags-table (&optional directory)
  "Make tags file to the current project. With prefix arg, specify file path."
  (interactive (if current-prefix-arg
                   (list (read-directory-name "Make TAGS to: " nil nil t))
                 nil))
  (let* ((current-prefix-arg nil) ; reset as it might affect future commands
         (dir (if directory
                  directory
                (if-let ((proj-dir (projectile-project-root)))
                    proj-dir
                  (read-directory-name "Make TAGS to: " nil nil t))))
         (file (concat dir "TAGS")))
    (shell-command (format "ctags -e -R -f \"%s\" \"%s\"" file dir)
                   "*ctags Command Output*")
    (visit-tags-table file)
    (message "Made and visited TAGS: %s" file)))

(global-set-key (kbd "C-c T") #'tkareine/make-tags-table)

(defun tkareine/visit-tags-table (&optional tags-file)
  "Visit TAGS file of the current project. With prefix arg, specify the file path."
  (interactive (if current-prefix-arg
                   (list (read-file-name "Visit TAGS: " nil nil t))
                 nil))
  (let ((current-prefix-arg nil) ; reset as it might affect future commands
        (file (if tags-file
                  tags-file
                (if-let ((proj-file (tkareine/try-projectile-expand-root-readable-file-p "TAGS" #'file-readable-p)))
                    proj-file
                  (read-file-name "Visit TAGS: " nil nil t)))))
    (visit-tags-table file)
    (message "Visited TAGS: %s" file)))

(global-set-key (kbd "C-c t") #'tkareine/visit-tags-table)

;; Company: auto completion
(require 'company)
(global-company-mode)

;; Helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c C-SPC") #'helm-all-mark-rings)
(global-set-key (kbd "C-c b")     #'helm-resume)
(global-set-key (kbd "C-c h")     #'helm-command-prefix)
(global-set-key (kbd "C-c o")     #'helm-occur)
(global-set-key (kbd "C-c r")     #'helm-register)
(global-set-key (kbd "C-x C-f")   #'helm-find-files)
(global-set-key (kbd "C-x b")     #'helm-mini)
(global-set-key (kbd "M-x")       #'helm-M-x)
(global-set-key (kbd "M-y")       #'helm-show-kill-ring)
(global-set-key (kbd "s-.")       #'helm-semantic-or-imenu)
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
;; Helm: always show full buffer name in helm-M-x
(customize-set-variable 'helm-buffer-max-length nil)
;; Helm: enable
(helm-mode t)

;; Helm-ag
(customize-set-variable 'helm-ag-insert-at-point 'symbol)
(global-set-key (kbd "C-c C-s") #'helm-do-ag)
(global-set-key (kbd "C-c S")   #'helm-ag-this-file)

;; Projectile
(require 'helm-projectile)
(global-set-key (kbd "C-c d") #'helm-projectile-find-dir)
(global-set-key (kbd "C-c f") #'helm-projectile-find-file)
(global-set-key (kbd "C-c g") #'helm-projectile-find-file-dwim)
(global-set-key (kbd "C-c s") #'helm-projectile-ag)
(global-set-key (kbd "C-c v") #'helm-projectile-find-other-file)
(customize-set-variable 'projectile-completion-system 'helm)
(customize-set-variable 'projectile-switch-project-action 'helm-projectile)
(add-to-list 'projectile-other-file-alist '("html" "js" "css"))
(add-to-list 'projectile-other-file-alist '("js" "html" "css"))
(add-to-list 'projectile-other-file-alist '("css" "html" "js"))
(projectile-mode)
(helm-projectile-on)

;; UndoTree
(global-undo-tree-mode)

;; Ag: enable search highlighting
(customize-set-variable 'ag-highlight-search t)
(customize-set-variable 'ag-project-root-function
                        (lambda (_dir) (projectile-project-root)))
(global-set-key (kbd "C-c A")   #'ag)
(global-set-key (kbd "C-c a")   #'ag-project-regexp)
(global-set-key (kbd "C-c C-a") #'ag-regexp)

;; GitGutter
(global-git-gutter-mode t)
(customize-set-variable 'git-gutter:lighter " GG")

;; Magit
(global-set-key (kbd "C-x g") #'magit-status)

;; Paredit
(eval-after-load "paredit"
  (lambda ()
    (define-key paredit-mode-map (kbd "C-<left>")  nil)
    (define-key paredit-mode-map (kbd "C-<right>") nil)
    (define-key paredit-mode-map (kbd "M-<left>")  #'paredit-forward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-<right>") #'paredit-forward-slurp-sexp)))

(add-hook 'clojure-mode-hook                     #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'ielm-mode-hook                        #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'paredit-mode)
(add-hook 'lisp-mode-hook                        #'paredit-mode)
(add-hook 'scheme-mode-hook                      #'paredit-mode)

;; Dash
(if (eq system-type 'darwin)
    (global-set-key (kbd "C-c ?") #'dash-at-point))

;; Expand-reqion
(global-set-key (kbd "C-=") #'er/expand-region)

;; Flycheck: enable globally
(add-hook 'after-init-hook #'global-flycheck-mode)
(customize-set-variable 'flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Which-key: enable globally
(which-key-mode)

;; Highlight-symbol
(customize-set-variable 'highlight-symbol-colors '("orange3"
                                                   "DeepPink3"
                                                   "cyan4"
                                                   "MediumPurple3"
                                                   "SpringGreen4"
                                                   "DarkOrange3"
                                                   "HotPink3"
                                                   "RoyalBlue1"
                                                   "OliveDrab"))
(customize-set-variable 'highlight-symbol-foreground-color "#DCDCCC")
(global-set-key (kbd "C-<f5>") #'highlight-symbol)
(global-set-key (kbd "<f5>")   #'highlight-symbol-next)
(global-set-key (kbd "S-<f5>") #'highlight-symbol-prev)
(global-set-key (kbd "M-<f5>") #'highlight-symbol-query-replace)
