;; Minibuffer
(customize-set-variable 'enable-recursive-minibuffers t)

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
(customize-set-variable 'recentf-exclude
                        (list
                         (concat "\\`" (expand-file-name (tkareine/dotfile-path "recentf")) "\\'")
                         (concat "\\`" (expand-file-name (tkareine/dotfile-path "elpa")) "/.*-autoloads.elc?\\'")))

;; Recentf: save the list of recent files periodically. Normally,
;; Recentf saves the list when Emacs exits cleanly. If Emacs crashes,
;; that save is probably not done.
(defun tkareine/recentf-save-list-silent ()
  (let ((inhibit-message t))
    (recentf-save-list)))

(run-at-time (* 5 60) (* 5 60) #'tkareine/recentf-save-list-silent)

;; Recentf: enable globally
(recentf-mode)

;; Hippie-expand
(global-set-key (kbd "s-SPC") #'hippie-expand)

;; ggtags: use GNU Global with Emacs's xref facility

(require 'ggtags)

(defun tkareine/make-gtags (rootdir)
  "Make gtags files to the current project.

If called with a prefix, specify the directory to make gtags files for."
  (interactive (cl-flet ((read-dir ()
                                   (read-directory-name "Make GTAGS to: " nil nil t)))
                 (let ((dir (if current-prefix-arg
                                (read-dir)
                              (if-let ((proj-dir (projectile-project-root)))
                                  proj-dir
                                (read-dir)))))
                    (list dir))))
  (let ((current-prefix-arg nil)) ; reset as it might affect future commands
    (ggtags-create-tags rootdir)))

(customize-set-variable 'ggtags-process-environment '("GTAGSLABEL=default"))

;; ggtags: don't change `mode-line-buffer-identification', because we
;; show project root dir in the mode line with projectile
(setq ggtags-mode-line-project-name nil)

(add-hook 'enh-ruby-mode-hook #'ggtags-mode)
(add-hook 'js2-mode-hook      #'ggtags-mode)
(add-hook 'less-css-mode-hook #'ggtags-mode)
(add-hook 'scss-mode-hook     #'ggtags-mode)

(global-set-key (kbd "C-c T") #'tkareine/make-gtags)
(global-set-key (kbd "C-c t") #'ggtags-find-tag-dwim)

;; Company: enable auto completion globally
(require 'company)
(global-company-mode)

;; Company: don't lowercase completion candidates (dabbrev backend)
(customize-set-variable 'company-dabbrev-downcase nil)

;; Company: ignore case when collecting completion candidates and copy
;; candidate verbatim (dabbrev and dabbrev-code backends)
(customize-set-variable 'company-dabbrev-ignore-case t)
(customize-set-variable 'company-dabbrev-code-ignore-case t)

;; Ivy, Counsel, and Swiper
(customize-set-variable 'ivy-use-virtual-buffers t)
(customize-set-variable 'ivy-count-format "(%d/%d) ")
(customize-set-variable 'ivy-height 20)

(custom-set-faces '(ivy-current-match   ((t (:weight bold
                                             :underline nil
                                             :foreground nil
                                             :background "grey15"))))
                  '(ivy-action          ((t (:weight bold
                                             :foreground "#f0dfaf"))))
                  '(ivy-subdir          ((t (:weight bold
                                             :foreground "#ffffef"))))
                  '(ivy-virtual         ((t (:foreground "grey70"))))
                  '(ivy-remote          ((t (:foreground "#cc9393"))))
                  '(ivy-modified-buffer ((t (:weight bold
                                             :foreground "#bfebbf")))))

(global-set-key (kbd "C-c b")   #'ivy-resume)
(global-set-key (kbd "C-c g")   #'counsel-git)
(global-set-key (kbd "C-c j")   #'counsel-git-grep)
(global-set-key (kbd "C-c l")   #'counsel-locate)
(global-set-key (kbd "C-c m")   #'counsel-bookmark)
(global-set-key (kbd "C-c s")   #'counsel-ag)
(global-set-key (kbd "C-h f")   #'counsel-describe-function)
(global-set-key (kbd "C-h i")   #'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h l")   #'counsel-find-library)
(global-set-key (kbd "C-h u")   #'counsel-unicode-char)
(global-set-key (kbd "C-h v")   #'counsel-describe-variable)
(global-set-key (kbd "C-s")     #'swiper)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)
(global-set-key (kbd "M-x")     #'counsel-M-x)
(global-set-key (kbd "M-y")     #'counsel-yank-pop)
(global-set-key (kbd "s-.")     #'counsel-semantic-or-imenu)

(define-key minibuffer-local-map (kbd "C-r") #'counsel-minibuffer-history)

(require 'ivy)
(require 'counsel)
(ivy-add-actions 'counsel-find-file '(("D" delete-file "delete")))

(ivy-mode 1)

;; Projectile
(global-set-key (kbd "C-c d") #'counsel-projectile-find-dir)
(global-set-key (kbd "C-c f") #'counsel-projectile-find-file)
(global-set-key (kbd "C-c s") #'counsel-projectile-ag)
(customize-set-variable 'projectile-completion-system 'ivy)
(require 'projectile)
(add-to-list 'projectile-other-file-alist '("html" "js" "css"))
(add-to-list 'projectile-other-file-alist '("js" "html" "css"))
(add-to-list 'projectile-other-file-alist '("css" "html" "js"))
(projectile-mode)
(counsel-projectile-mode)

;; UndoTree
(global-undo-tree-mode)

;; Ag
(global-set-key (kbd "C-c A")   #'ag)
(global-set-key (kbd "C-c a")   #'ag-project-regexp)
(global-set-key (kbd "C-c C-a") #'ag-regexp)

;; Ag: enable search highlighting
(customize-set-variable 'ag-highlight-search t)

;; Ag: projectile determines project root
(customize-set-variable 'ag-project-root-function
                        (lambda (_dir) (projectile-project-root)))

;; GitGutter
(global-git-gutter-mode t)
(customize-set-variable 'git-gutter:lighter " GG")

;; Magit
(global-set-key (kbd "C-x g") #'magit-status)
(customize-set-variable 'magit-completing-read-function 'ivy-completing-read)
(custom-set-faces '(magit-diff-context ((t (:background "grey25"))))
                  '(magit-diff-context-highlight ((t (:background "grey32"))))
                  '(magit-diff-hunk-heading ((t (:background "#3e5f76"))))
                  '(magit-diff-hunk-heading-highlight ((t (:background "#619abf")))))

;; Magit: disable magit-auto-revert-mode, because we're using
;; global-auto-revert-mode
(customize-set-variable 'magit-auto-revert-mode nil)

;; Magit: disable Emacs' Version Control interface
;; (customize-set-variable 'vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg Mtn))
(customize-set-variable 'vc-handled-backends nil)

;; Paredit
(defun tkareine/paredit-mode-customizations ()
  (define-key paredit-mode-map (kbd "C-<left>")  nil)
  (define-key paredit-mode-map (kbd "C-<right>") nil)
  (define-key paredit-mode-map (kbd "M-<left>")  #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-<right>") #'paredit-forward-slurp-sexp))

(eval-after-load 'paredit #'tkareine/paredit-mode-customizations)

(add-hook 'clojure-mode-hook                     #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'ielm-mode-hook                        #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'paredit-mode)
(add-hook 'lisp-mode-hook                        #'paredit-mode)
(add-hook 'scheme-mode-hook                      #'paredit-mode)

(define-key prog-mode-map (kbd "C-c (") #'paredit-mode)

;; CIDER
(customize-set-variable 'cider-eval-result-prefix ";; => ")
(customize-set-variable 'cider-repl-result-prefix ";; => ")
(customize-set-variable 'cider-repl-history-file "~/.cider_history")

;; CIDER: attempt to use the symbol at point as input for
;; `cider-find-var', and only prompt if that throws an error
(customize-set-variable 'cider-prompt-for-symbol nil)

;; CIDER: I want to inject dependencies manually via
;; `~/.lein/profiles.clj'. Otherwise Leiningen's `:pedantic? :abort'
;; setting causes `lein repl' to abort due to overriding version of
;; `org.clojure/tools.nrepl'.
(customize-set-variable 'cider-inject-dependencies-at-jack-in nil)

(custom-set-faces '(cider-result-overlay-face ((t (:background "grey30")))))

;; CIDER: shorten mode line info
(customize-set-variable 'cider-mode-line '(:eval (cider--modeline-info)))

;; CIDER: add related info to mode line
(defun tkareine/cider-mode-customizations ()
  (add-to-list 'tkareine/minor-mode-alist '(cider-popup-buffer-mode (" cider-tmp")))
  (add-to-list 'tkareine/minor-mode-alist '(cider-auto-test-mode (cider-mode " Test")))
  (add-to-list 'tkareine/minor-mode-alist '(cider-mode cider-mode-line))
  (add-to-list 'tkareine/minor-mode-alist '(cider--debug-mode " DEBUG")))

(eval-after-load 'cider-mode #'tkareine/cider-mode-customizations)

(defun tkareine/cider-mode-hook ()
  (local-set-key (kbd "C-c B")       #'cider-connection-browser)
  (local-set-key (kbd "C-c C-v C-b") #'cider-eval-buffer)
  (local-set-key (kbd "C-c M-R")     #'cider-restart))

(add-hook 'cider-mode-hook #'tkareine/cider-mode-hook)
(add-hook 'cider-repl-mode-hook #'tkareine/cider-mode-hook)

;; Dash
(if (eq system-type 'darwin)
    (dolist (m (list text-mode-map prog-mode-map))
      (define-key m (kbd "C-c ?") #'dash-at-point)))

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
(customize-set-variable 'highlight-symbol-foreground-color "#dcdccc")

(global-set-key (kbd "C-<f5>") #'highlight-symbol)
(global-set-key (kbd "<f5>")   #'highlight-symbol-next)
(global-set-key (kbd "S-<f5>") #'highlight-symbol-prev)
(global-set-key (kbd "M-<f5>") #'highlight-symbol-query-replace)
