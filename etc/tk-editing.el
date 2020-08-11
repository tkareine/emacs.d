;; -*- lexical-binding: t; -*-

;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Hard wrapping at column number
(customize-set-variable 'fill-column 72)

;; Default indentation
(customize-set-variable 'standard-indent 2)

;; Add missing newline to file automatically when saving
(customize-set-variable 'require-final-newline t)

;; Use text-mode for *scratch* buffer
(customize-set-variable 'initial-major-mode 'text-mode)

;; Default major-mode
(customize-set-variable 'major-mode 'text-mode)

;; Do not insert tabs in place of multiple spaces when formatting a region
(customize-set-variable 'indent-tabs-mode nil)

;; Allow downcase-region (C-x C-l), upcase-region (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; macOS: allow entering special chars via Option key
(customize-set-variable 'mac-option-modifier nil)

;; macOS: use Cmd key as Meta modifier
(customize-set-variable 'mac-command-modifier 'meta)

;; macOS: use fn key as Super modifier
(customize-set-variable 'mac-function-modifier 'super)

;; Typing text replaces active selection
(delete-selection-mode)

;; Enable auto pairing of brackets and quotation marks
(electric-pair-mode)

;; Revert file buffer if changed externally
(global-auto-revert-mode)

;; Save typing chars when answering yes-or-no-p questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; macOS: Use `mdfind' for locate
(when (eq system-type 'darwin)
  (customize-set-variable 'locate-command "mdfind"))

;; Apropos commands perform more extensive searches than default
(customize-set-variable 'apropos-do-all t)

;; Enable narrowing to a region (hiding warning text).
(put 'narrow-to-region 'disabled nil)

(defun tk-editing/back-to-indentation-or-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

Adapted from
`https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/'"
  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                #'tk-editing/back-to-indentation-or-move-beginning-of-line)

;;; Navigation by moving in steps of 5

(defun tk-editing/next-line-5 ()
  (interactive)
  (ignore-errors (forward-line 5)))

(global-set-key (kbd "C-n") #'tk-editing/next-line-5)

(defun tk-editing/previous-line-5 ()
  (interactive)
  (ignore-errors (forward-line -5)))

(global-set-key (kbd "C-p") #'tk-editing/previous-line-5)

(defun tk-editing/forward-char-5 ()
  (interactive)
  (ignore-errors (forward-char 5)))

(global-set-key (kbd "C-f") #'tk-editing/forward-char-5)

(defun tk-editing/backward-char-5 ()
  (interactive)
  (ignore-errors (backward-char 5)))

(global-set-key (kbd "C-b") #'tk-editing/backward-char-5)

;;; Editing

(defun tk-editing/join-line ()
  (interactive)
  (join-line -1))

(global-set-key (kbd "M-J") #'join-line)
(global-set-key (kbd "M-j") #'tk-editing/join-line)

(defun tk-editing/eol-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "S-<return>")    #'tk-editing/eol-newline-and-indent)
(global-set-key (kbd "s-<backspace>") #'delete-char)

;;; Commenting

(defun tk-editing/comment-or-uncomment-region-or-line ()
  "Comments or uncomments either the current line (if no region
active) or region, moving position if point is at the beginning
of region."
  (interactive)
  (require 'subr-x)
  (when-let ((region (tk-support/active-region-or-line)))
    (let* ((region-begin-pos (car region))
           (region-end-pos (cadr region))
           (current-pos (point))
           (current-line-begin-pos (line-beginning-position))
           (next-line-begin-pos (line-beginning-position 2)))
      (comment-or-uncomment-region region-begin-pos region-end-pos)
      (when (and (= current-line-begin-pos region-begin-pos)
                 (= current-pos region-begin-pos)
                 (>= next-line-begin-pos region-end-pos))
        (beginning-of-line 2)))))

(global-set-key (kbd "C-c C") #'comment-dwim)
(global-set-key (kbd "M-/")   #'tk-editing/comment-or-uncomment-region-or-line)

;;; Showing and handling whitespace

;; Visually indicate empty lines in buffer in the left fringe
(customize-set-variable 'indicate-empty-lines t)

;; Highlight trailing whitespaces in lines. Let's use this instead of
;; the similar feature in whitespace.el, because this marks trailing
;; whitespace in lines already highlighted with `whitespace-line' face.
(customize-set-variable 'show-trailing-whitespace t)

(customize-set-variable 'whitespace-style '(face
                                            tabs
                                            lines
                                            ;; don't include `trailing', see comment above
                                            space-after-tab::tab
                                            space-before-tab::tab
                                            tab-mark))

(custom-set-faces '(whitespace-tab ((t (:background "grey30"))))
                  '(whitespace-line ((t (:background "#66494a" :foreground nil)))))

(global-whitespace-mode)

(defun tk-editing/toggle-show-trailing-whitespace ()
  (interactive)
  (customize-set-variable 'show-trailing-whitespace (eq show-trailing-whitespace nil)))

(global-set-key (kbd "C-x W")         #'tk-editing/toggle-show-trailing-whitespace)
(global-set-key (kbd "C-x t")         #'delete-trailing-whitespace)
(global-set-key (kbd "C-x w")         #'whitespace-mode)
(global-set-key (kbd "M-S-SPC")       #'cycle-spacing)

;;; Global navigation and window management

(global-set-key (kbd "S-<down>")  #'windmove-down)
(global-set-key (kbd "S-<left>")  #'windmove-left)
(global-set-key (kbd "S-<right>") #'windmove-right)
(global-set-key (kbd "S-<up>")    #'windmove-up)
(global-set-key (kbd "s-0")       #'delete-frame)
(global-set-key (kbd "s-1")       #'delete-other-frames)
(global-set-key (kbd "s-2")       #'make-frame-command)
(global-set-key (kbd "s-<down>")  #'scroll-up)
(global-set-key (kbd "s-<left>")  #'beginning-of-buffer)
(global-set-key (kbd "s-<right>") #'end-of-buffer)
(global-set-key (kbd "s-<up>")    #'scroll-down)

;; Force your learning to avoid using M-<left|right> for movement
;; between words
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))

;;; Kill ring

(defun tk-editing/kill-ring-save (beg end)
  "Like `kill-ring-save', but when called interactively with no
active region, copy the current line instead."
  (interactive (tk-support/active-region-or-line))
  (kill-ring-save beg end))

(global-set-key [remap kill-ring-save] #'tk-editing/kill-ring-save)

(defun tk-editing/kill-region (beg end)
  "Like `kill-region', but when called interactively with no
active region, kill the current line instead."
  (interactive (tk-support/active-region-or-line))
  (kill-region beg end))

(global-set-key [remap kill-region] #'tk-editing/kill-region)

(defun tk-editing/file-path-to-clipboard ()
  "Copy the current file name to the clipboard."
  (interactive)
  (let ((path (expand-file-name (or (buffer-file-name) default-directory))))
    (when path
      (let ((select-enable-clipboard t)) (gui-select-text path))
      (kill-new path)
      (message path))))

(global-set-key (kbd "C-c P")         #'tk-editing/file-path-to-clipboard)
(global-set-key (kbd "M-<kp-delete>") #'kill-word)

;; Save clipboard strings into kill ring before replacing them
(customize-set-variable 'save-interprogram-paste-before-kill t)

;; Mouse yanking inserts at the point instead of the location of the click
(customize-set-variable 'mouse-yank-at-point t)

;; When killing, stop at subwords inside a CamelCase word
(add-hook 'prog-mode-hook #'subword-mode)

;;; CUA: enhanced rectangle support

(cua-selection-mode 1)

;;; Expand-reqion

(use-package expand-region
  :ensure t

  :bind
  (("C-=" . er/expand-region)))

;;; Smartparens

(use-package smartparens
  :ensure t

  :demand

  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (sp-use-paredit-bindings)
  (add-to-list 'tk-looks/minor-mode-alist
               '(smartparens-mode (" SP" (:eval (if smartparens-strict-mode "/s" "")))))

  :bind
  (:map smartparens-mode-map
        ("C-<left>" .   nil)
        ("C-<right>"  . nil)
        ("C-M-<down>" . sp-down-sexp)
        ("C-M-<up>"   . sp-up-sexp)
        ("C-M-a"      . sp-beginning-of-sexp)
        ("C-M-e"      . sp-end-of-sexp)
        ("C-M-k"      . sp-kill-sexp)
        ("C-M-n"      . sp-next-sexp)
        ("C-M-p"      . sp-backward-sexp)
        ("C-M-t"      . sp-transpose-sexp)
        ("C-S-<down>" . sp-backward-down-sexp)
        ("C-S-<up>"   . sp-backward-up-sexp)
        ("C-c )"      . smartparens-strict-mode)
        ("M-<left>"   . sp-forward-barf-sexp)
        ("M-<right>"  . sp-forward-slurp-sexp)))

;;; Registers

(global-set-key (kbd "M-[") #'point-to-register)
(global-set-key (kbd "M-]") #'jump-to-register)

;;; Other key bindings

(global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)
(global-set-key (kbd "C-x C-b")     #'ibuffer)
(global-set-key (kbd "C-c U")       #'browse-url-at-point)

;;; Minibuffer

(customize-set-variable 'enable-recursive-minibuffers t)

;; Remove duplicate elements from history lists
(customize-set-variable 'history-delete-duplicates t)

;;; Tramp

(customize-set-variable 'tramp-default-method "ssh")

;;; Dired

(customize-set-variable 'dired-listing-switches "-alh")

;; Allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; DiredX: for Dired Jump
(with-eval-after-load 'dired (require 'dired-x))

;;; Uniquify: append dir name to buffers with similar filenames

(require 'uniquify)

(customize-set-variable 'uniquify-buffer-name-style 'forward)

;;; Saveplace: save point location in the buffer when revisiting the buffer

(customize-set-variable 'save-place-file (tk-init/user-emacs-path "saveplace"))
(customize-set-variable 'savehist-file (tk-init/user-emacs-path "savehist"))

(save-place-mode)
(savehist-mode)

;;; Recentf: shows list of recently opened files

(require 'recentf)

(customize-set-variable 'recentf-save-file (tk-init/user-emacs-path "recentf"))

;; Exclude recentf save file and Emacs ELPA autoloads
(customize-set-variable 'recentf-exclude
                        (list
                         (concat "\\`" (tk-init/user-emacs-path "recentf") "\\'")
                         (concat "\\`" (tk-init/user-emacs-path "elpa") "/.*-autoloads.elc?\\'")))

(defun tk-editing/recentf-save-list-silent ()
  "Save the list of recent files periodically. Normally, recentf saves
the list when Emacs exits cleanly. If Emacs crashes, that save is
probably not done."
  (let ((inhibit-message t))
    (recentf-save-list)))

(run-at-time (* 5 60) (* 5 60) #'tk-editing/recentf-save-list-silent)

(recentf-mode)

;;; Hippie-expand

(global-set-key (kbd "s-SPC") #'hippie-expand)

;;; UndoTree

(use-package undo-tree
  :ensure t

  :config
  (global-undo-tree-mode)

  :bind
  (("C-x u" . undo-tree-visualize)))

;;; Projectile

(use-package projectile
  :ensure t

  :demand

  :config
  (dolist (l '(("js" "scss" "less" "css" "html")
               ("jsx" "scss" "less" "css" "html")
               ("scss" "jsx" "js" "html")
               ("less" "jsx" "js" "html")
               ("css" "jsx" "js" "html")))
    (add-to-list 'projectile-other-file-alist l))

  (projectile-register-project-type 'npm
                                    '("package.json")
                                    :compile "npm install"
                                    :test "npm test"
                                    :test-suffix ".test")

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :bind
  (("C-c D" . projectile-dired)
   ("C-c F" . projectile-find-file-dwim)
   ("C-c i" . projectile-toggle-between-implementation-and-test)
   ("C-c o" . projectile-find-other-file))

  :custom
  (projectile-completion-system 'ivy))

;;; Ivy, Counsel, and Swiper

(use-package ivy
  :ensure t

  :demand

  :config
  (ivy-mode)

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-format-function #'ivy-format-function-arrow)
  (ivy-height 20)
  (ivy-magic-slash-non-match-action nil "Don't cd to existing directory when appending \"/\", allowing creating new buffer in new directory")

  :custom-face
  (ivy-current-match   ((t (:weight bold
                            :underline nil
                            :foreground "#f0dfaf"
                            :background "grey10"))))
  (ivy-action          ((t (:weight bold
                            :foreground "#f0dfaf"))))
  (ivy-subdir          ((t (:weight bold
                            :foreground "#ffffef"))))
  (ivy-virtual         ((t (:foreground "grey70"))))
  (ivy-remote          ((t (:foreground "#cc9393"))))
  (ivy-modified-buffer ((t (:weight bold
                            :foreground "#bfebbf"))))

  :bind
  (("C-c b" . ivy-resume)))

(use-package swiper
  :ensure t

  :bind
  (("C-S-s" . swiper)))

(use-package counsel
  :ensure t

  :demand

  :config
  (define-key minibuffer-local-map (kbd "C-r") #'counsel-minibuffer-history)

  (defun tk-editing/dired-open-directory-of-file (file)
    (dired (file-name-directory (directory-file-name file))))

  (ivy-add-actions #'counsel-find-file
                   '(("D"
                      tk-editing/dired-open-directory-of-file
                      "open file's directory")))

  :custom
  (counsel-find-file-at-point t)

  :bind
  (("C-c g"   . counsel-git)
   ("C-c j"   . counsel-git-grep)
   ("C-c l"   . counsel-locate)
   ("C-c m"   . counsel-bookmark)
   ("C-h b"   . counsel-descbinds)
   ("C-h f"   . counsel-describe-function)
   ("C-h i"   . counsel-info-lookup-symbol)
   ("C-h l"   . counsel-find-library)
   ("C-h u"   . counsel-unicode-char)
   ("C-h v"   . counsel-describe-variable)
   ("C-x C-f" . counsel-find-file)
   ("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("s-."     . counsel-semantic-or-imenu)))

(use-package counsel-projectile
  :ensure t

  :config
  (defun tk-editing/dired-open-directory-of-project-file (file)
    (tk-editing/dired-open-directory-of-file (concat (projectile-project-root) file)))

  (ivy-add-actions #'counsel-projectile-find-file
                   '(("D"
                      tk-editing/dired-open-directory-of-project-file
                      "open file's directory")))

  (counsel-projectile-mode)

  :bind
  (("C-c d" . counsel-projectile-find-dir)
   ("C-c f" . counsel-projectile-find-file)
   ("C-c s" . counsel-projectile-rg)))

;;; Deadgrep interface for ripgrep

(use-package deadgrep
  :ensure t

  :config
  ;; Projectile determines project root
  (setq deadgrep-project-root-function #'projectile-project-root)

  (defun tk-editing/deadgrep-show-result-other-window ()
    "Show the result in another window at point, keeping the
current search result window."
    (interactive)
    (let ((buf (car-safe (deadgrep--buffers))))
      (deadgrep-visit-result-other-window)
      (when buf
        (pop-to-buffer buf))))

  :bind
  (("C-c a" . deadgrep)
   :map deadgrep-mode-map
   ("O"       . tk-editing/deadgrep-show-result-other-window)
   ("C-c C-f" . next-error-follow-minor-mode)))

;;; Symbol-overlay

(use-package symbol-overlay
  :ensure t

  :config
  (let ((symbol-overlay-faces
         (cl-loop for (face . color) in '((symbol-overlay-face-1 . "orange3")
                                          (symbol-overlay-face-2 . "DeepPink3")
                                          (symbol-overlay-face-3 . "cyan4")
                                          (symbol-overlay-face-4 . "MediumPurple3")
                                          (symbol-overlay-face-5 . "SpringGreen4")
                                          (symbol-overlay-face-6 . "DarkOrange3")
                                          (symbol-overlay-face-7 . "HotPink3")
                                          (symbol-overlay-face-8 . "RoyalBlue1"))
                  collect `(,face ((t (:background ,color)))))))
    (apply 'custom-set-faces symbol-overlay-faces))

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-N") #'symbol-overlay-switch-forward)
    (define-key map (kbd "M-P") #'symbol-overlay-switch-backward)
    (define-key map (kbd "M-e") #'symbol-overlay-echo-mark)
    (define-key map (kbd "M-n") #'symbol-overlay-jump-next)
    (define-key map (kbd "M-p") #'symbol-overlay-jump-prev)
    (define-key map (kbd "M-q") #'symbol-overlay-query-replace)
    (define-key map (kbd "M-r") #'symbol-overlay-rename)
    (define-key map (kbd "M-s") #'symbol-overlay-isearch-literally)
    (define-key map (kbd "M-t") #'symbol-overlay-toggle-in-scope)
    (define-key map (kbd "M-w") #'symbol-overlay-save-symbol)
    (setq symbol-overlay-map map))

  :bind
  (("s-O" . symbol-overlay-remove-all)
   ("s-o" . symbol-overlay-put)))
