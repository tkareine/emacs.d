;; -*- lexical-binding: t; -*-

(require 'tk-support)

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

;; macOS: use Cmd as Meta modifier
(customize-set-variable 'mac-command-modifier 'meta)

;; Bind Ns key to Super modifer
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
  (interactive)
  (let ((region (tk-support/active-region-or-line)))
    (when region
      (let ((rbegin (car region))
            (rend (cadr region)))
        (comment-or-uncomment-region rbegin rend)))))

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

(defun tk-editing/kill-ring-save-advice (adviced &rest arguments)
  "When called interactively with no active region, copy current line instead."
  (interactive (tk-support/active-region-or-line))
  (apply adviced arguments))

(advice-add #'kill-ring-save :around #'tk-editing/kill-ring-save-advice)

(defun tk-editing/kill-region-advice (adviced &rest arguments)
  "When called interactively with no active region, kill current line instead."
  (interactive (tk-support/active-region-or-line))
  (apply adviced arguments))

(advice-add #'kill-region :around #'tk-editing/kill-region-advice)

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

(global-set-key (kbd "C-=") #'er/expand-region)

;;; Smartparens

(require 'smartparens-config)

(smartparens-global-mode)
(show-smartparens-global-mode)
(sp-use-paredit-bindings)

(defun tk-dev/smartparens-mode-customizations ()
  (define-key smartparens-mode-map (kbd "C-<left>")  nil)
  (define-key smartparens-mode-map (kbd "C-<right>") nil)
  (define-key smartparens-mode-map (kbd "C-M-k")     #'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-c )")     #'smartparens-strict-mode)
  (define-key smartparens-mode-map (kbd "M-<left>")  #'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-<right>") #'sp-forward-slurp-sexp)

  (add-to-list 'tk-looks/minor-mode-alist '(smartparens-mode (" SP" (:eval (if smartparens-strict-mode "/s" ""))))))

(eval-after-load 'smartparens #'tk-dev/smartparens-mode-customizations)

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

;; Allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; DiredX: for Dired Jump
(require 'dired-x)

;;; Uniquify: append dir name to buffers with similar filenames

(require 'uniquify)

(customize-set-variable 'uniquify-buffer-name-style 'forward)

;;; Saveplace: save point location in the buffer when revisiting the buffer

(customize-set-variable 'save-place-file (tk-support/dotfile-path "saveplace"))
(customize-set-variable 'savehist-file (tk-support/dotfile-path "savehist"))

(save-place-mode)
(savehist-mode)

;;; Recentf: shows list of recently opened files

(require 'recentf)

(customize-set-variable 'recentf-save-file (tk-support/dotfile-path "recentf"))

;; Exclude recentf save file and Emacs ELPA autoloads
(customize-set-variable 'recentf-exclude
                        (list
                         (concat "\\`" (tk-support/dotfile-path "recentf") "\\'")
                         (concat "\\`" (tk-support/dotfile-path "elpa") "/.*-autoloads.elc?\\'")))

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

(global-undo-tree-mode)

;;; Ivy, Counsel, and Swiper

(require 'ivy)
(require 'counsel)

(customize-set-variable 'ivy-use-virtual-buffers t)
(customize-set-variable 'ivy-count-format "(%d/%d) ")
(customize-set-variable 'ivy-format-function #'ivy-format-function-arrow)
(customize-set-variable 'ivy-height 20)
(customize-set-variable 'counsel-find-file-at-point t)

(custom-set-faces '(ivy-current-match   ((t (:weight bold
                                             :underline nil
                                             :foreground "#f0dfaf"
                                             :background "grey10"))))
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
(global-set-key (kbd "C-h b")   #'counsel-descbinds)
(global-set-key (kbd "C-h f")   #'counsel-describe-function)
(global-set-key (kbd "C-h i")   #'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h l")   #'counsel-find-library)
(global-set-key (kbd "C-h u")   #'counsel-unicode-char)
(global-set-key (kbd "C-h v")   #'counsel-describe-variable)
(global-set-key (kbd "C-S-s")   #'swiper)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)
(global-set-key (kbd "M-x")     #'counsel-M-x)
(global-set-key (kbd "M-y")     #'counsel-yank-pop)
(global-set-key (kbd "s-.")     #'counsel-semantic-or-imenu)

(define-key minibuffer-local-map (kbd "C-r") #'counsel-minibuffer-history)

(ivy-add-actions 'counsel-find-file '(("D" dired-delete-file "delete")))

(ivy-mode)

;;; Projectile

(require 'projectile)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "C-c D") #'projectile-dired)
(global-set-key (kbd "C-c F") #'projectile-find-file-dwim)
(global-set-key (kbd "C-c d") #'counsel-projectile-find-dir)
(global-set-key (kbd "C-c f") #'counsel-projectile-find-file)
(global-set-key (kbd "C-c i") #'projectile-toggle-between-implementation-and-test)
(global-set-key (kbd "C-c o") #'projectile-find-other-file)
(global-set-key (kbd "C-c s") #'counsel-projectile-ag)

(customize-set-variable 'projectile-completion-system 'ivy)

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

(counsel-projectile-mode)

;;; Ag

(global-set-key (kbd "C-c A")   #'ag)
(global-set-key (kbd "C-c a")   #'ag-project-regexp)
(global-set-key (kbd "C-c C-a") #'ag-regexp)

;; Include hidden files to default arguments
(customize-set-variable 'ag-arguments '("--smart-case" "--stats" "--hidden"))

;; Highlight matches in compilation buffer
(customize-set-variable 'ag-highlight-search t)

;; Projectile determines project root
(customize-set-variable 'ag-project-root-function
                        (lambda (_dir) (projectile-project-root)))

;;; Symbol-overlay

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

(defun tk-editing/symbol-overlay-customizations ()
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
    (setq symbol-overlay-map map)))

(eval-after-load 'symbol-overlay #'tk-editing/symbol-overlay-customizations)

(global-set-key (kbd "s-O") #'symbol-overlay-remove-all)
(global-set-key (kbd "s-o") #'symbol-overlay-put)
