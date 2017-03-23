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

;; Minibuffer history: remove duplicate elements from history lists
(customize-set-variable 'history-delete-duplicates t)

;; Saveplace: save point location in the buffer when revisiting the buffer
(require 'saveplace)
(customize-set-variable 'save-place t)
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
(defvar tkareine/tags-command-history
  nil
  "History list for the tags command asked in `tkareine/make-tags-table'.")

(defun tkareine/make-tags-table (command directory)
  "Make TAGS file to the current project.

If called with a prefix, specify the directory to make the tags file for."
  (interactive (let ((cmd (read-from-minibuffer "tags command: "
                                                "ctags -e -R "
                                                nil
                                                nil
                                                'tkareine/tags-command-history))
                     (dir (if current-prefix-arg
                              (read-directory-name "Make TAGS to: " nil nil t)
                            (if-let ((proj-dir (projectile-project-root)))
                                proj-dir
                              (read-directory-name "Make TAGS to: " nil nil t)))))
                 (list cmd dir)))
  (let* ((current-prefix-arg nil) ; reset as it might affect future commands
         (file (concat directory "TAGS"))
         (full-cmd (format "%s -f \"%s\" \"%s\"" command file directory)))
    (shell-command full-cmd "*tags Command Output*")
    (visit-tags-table file)
    (message "Made and visited TAGS: %s" full-cmd)))

(global-set-key (kbd "C-c T") #'tkareine/make-tags-table)

(defun tkareine/visit-tags-table (tags-file)
  "Visit TAGS file of the current project.

If called with a prefix, specify the file path."
  (interactive (let ((tags-file (if current-prefix-arg
                                    (read-file-name "Visit TAGS: " nil nil t)
                                  (if-let ((proj-file (tkareine/try-projectile-expand-root-file-p "TAGS" #'file-readable-p)))
                                      proj-file
                                    (read-file-name "Visit TAGS: " nil nil t)))))
                 (list tags-file)))
  (let ((current-prefix-arg nil)) ; reset as it might affect future commands
    (visit-tags-table tags-file)
    (message "Visited TAGS: %s" tags-file)))

(global-set-key (kbd "C-c t") #'tkareine/visit-tags-table)

;; Company: enable auto completion globally
(require 'company)
(global-company-mode)
;; Company: don't lowercase completion candidates (dabbrev backend)
(customize-set-variable 'company-dabbrev-downcase nil)
;; Company: ignore case when collecting completion candidates and copy
;; candidate verbatim (dabbrev and dabbrev-code backends)
(customize-set-variable 'company-dabbrev-ignore-case t)
(customize-set-variable 'company-dabbrev-code-ignore-case t)

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
;; Helm: switch bindings for TAB and C-z
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
