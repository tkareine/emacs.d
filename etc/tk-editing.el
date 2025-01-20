;; -*- lexical-binding: t; -*-

;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Hard wrapping at column number
(setq-default fill-column 72)

;; Do not insert tabs in place of multiple spaces when formatting a
;; region
(setq-default indent-tabs-mode nil)

;; Default indentation
(setq-default standard-indent 2)

;; Disable double space indicating the end of a sentence. Affects
;; commands such as `fill-paragraph' and `forward-sentence'.
(setq-default sentence-end-double-space nil)

;; Add missing newline to file automatically when saving
(setq-default require-final-newline t)

;; Use text-mode for *scratch* buffer
(setq-default initial-major-mode 'text-mode)

;; Default major-mode
(setq-default major-mode 'text-mode)

;; Allow downcase-region (C-x C-l), upcase-region (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; macOS: allow entering special chars via Option key
(setq-default mac-option-modifier nil)

;; macOS: use Cmd key as Meta modifier
(setq-default mac-command-modifier 'meta)

;; macOS: use fn key as Super modifier
(setq-default mac-function-modifier 'super)

;; Typing text replaces active selection
(delete-selection-mode 1)

;; Revert file buffer if changed externally
(global-auto-revert-mode 1)

;; Save typing chars when answering yes-or-no-p questions
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package locate
  :config
  ;; macOS: Use `mdfind' for locate
  (when (eq system-type 'darwin)
    (setq-default locate-command "mdfind")))

(use-package apropos
  :custom
  (apropos-do-all t "Apropos commands perform more extensive searches than default"))

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

(bind-key [remap move-beginning-of-line]
          #'tk-editing/back-to-indentation-or-move-beginning-of-line)

;;; Navigation by moving in steps of 5

(defun tk-editing/next-line-5 ()
  (interactive)
  (ignore-errors (forward-line 5)))

(defun tk-editing/previous-line-5 ()
  (interactive)
  (ignore-errors (forward-line -5)))

(defun tk-editing/forward-char-5 ()
  (interactive)
  (ignore-errors (forward-char 5)))

(defun tk-editing/backward-char-5 ()
  (interactive)
  (ignore-errors (backward-char 5)))

(bind-keys ("C-n" . tk-editing/next-line-5)
           ("C-p" . tk-editing/previous-line-5)
           ("C-f" . tk-editing/forward-char-5)
           ("C-b" . tk-editing/backward-char-5))

;;; Editing

(defun tk-editing/join-line ()
  (interactive)
  (join-line -1))

(bind-key "M-J" #'tk-editing/join-line)

(defun tk-editing/eol-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(bind-keys ("S-<return>"    . tk-editing/eol-newline-and-indent)
           ("s-<backspace>" . delete-char))

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

(bind-keys ("C-c C" . comment-dwim)
           ("M-/"   . tk-editing/comment-or-uncomment-region-or-line))

;;; Global navigation and window management

(bind-keys ("S-<down>"  . windmove-down)
           ("S-<left>"  . windmove-left)
           ("S-<right>" . windmove-right)
           ("S-<up>"    . windmove-up)
           ("s-0"       . delete-frame)
           ("s-1"       . delete-other-frames)
           ("s-2"       . make-frame-command)
           ("s-<down>"  . scroll-up)
           ("s-<left>"  . beginning-of-buffer)
           ("s-<right>" . end-of-buffer)
           ("s-<up>"    . scroll-down))

;; Force your learning to avoid using M-<left|right> for movement
;; between words
(bind-keys ("M-<left>"  . nil)
           ("M-<right>" . nil))

;;; Kill ring

(defun tk-editing/kill-ring-save (beg end)
  "Like `kill-ring-save', but when called interactively with no
active region, copy the current line instead."
  (interactive (tk-support/active-region-or-line))
  (kill-ring-save beg end))

(defun tk-editing/kill-region (beg end)
  "Like `kill-region', but when called interactively with no
active region, kill the current line instead."
  (interactive (tk-support/active-region-or-line))
  (kill-region beg end))

(defun tk-editing/file-path-to-clipboard ()
  "Copy the current file name to the clipboard."
  (interactive)
  (let ((path (expand-file-name (or (buffer-file-name) default-directory))))
    (when path
      (let ((select-enable-clipboard t)) (gui-select-text path))
      (kill-new path)
      (message path))))

(bind-keys ([remap kill-ring-save] . tk-editing/kill-ring-save)
           ("M-c"                  . tk-editing/kill-ring-save)
           ([remap kill-region]    . tk-editing/kill-region)
           ("C-c P"                . tk-editing/file-path-to-clipboard)
           ("M-<kp-delete>"        . kill-word))

;; Save clipboard strings into kill ring before replacing them
(setq-default save-interprogram-paste-before-kill t)

;; Mouse yanking inserts at the point instead of the location of the click
(setq-default mouse-yank-at-point t)

;; When killing, stop at subwords inside a CamelCase word
(add-hook 'prog-mode-hook #'subword-mode)

;;; CUA: enhanced rectangle support

(cua-selection-mode 1)

;;; Expand-reqion

(use-package expand-region
  :ensure t

  :bind
  (("M-[" . er/contract-region)
   ("M-]" . er/expand-region)))

;;; Smartparens
;;;
;;; `https://github.com/Fuco1/smartparens'

(use-package smartparens
  :ensure t

  :demand

  :custom
  (blink-matching-paren nil)

  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (add-to-list 'tk-looks/minor-mode-alist
               '(smartparens-mode (" SP" (:eval (if smartparens-strict-mode "/s" "")))))

  :bind
  (:map smartparens-mode-map
        ;; Navigation
        ("C-M-f"      . sp-forward-sexp)
        ("C-M-b"      . sp-backward-sexp)
        ("C-M-u"      . sp-backward-up-sexp)
        ("C-M-n"      . sp-up-sexp)
        ("C-M-<up>"   . sp-up-sexp)
        ("C-M-d"      . sp-down-sexp)
        ("C-M-<down>" . sp-down-sexp)
        ("C-M-p"      . sp-backward-down-sexp)
        ("C-M-a"      . sp-beginning-of-sexp)
        ("C-M-e"      . sp-end-of-sexp)
        ("C-M-k"      . sp-kill-sexp)
        ("C-M-n"      . sp-next-sexp)
        ("C-M-p"      . sp-backward-sexp)
        ("C-M-t"      . sp-transpose-sexp)
        ;; Editing
        ("C-M-j"       . sp-join-sexp)
        ("C-M-s"       . sp-splice-sexp)
        ("C-M-S-s"     . sp-split-sexp)
        ("C-M-<left>"  . sp-forward-barf-sexp)
        ("C-M-<right>" . sp-forward-slurp-sexp)
        ;; Misc
        ("C-c )" . smartparens-strict-mode)))

;;; Other key bindings

(bind-keys ("C-c C-c M-x" . execute-extended-command)
           ("C-x C-b"     . ibuffer)
           ("C-c U"       . browse-url-at-point))

;;; Minibuffer

(setq-default max-mini-window-height 0.2)

;; Disable recursive minibuffers, because resuming them is often
;; confusing
(setq-default enable-recursive-minibuffers nil)

;; Remove duplicate elements from history lists
(setq-default history-delete-duplicates t)

;;; Tramp

(use-package tramp
  :custom
  (tramp-default-method "ssh")

  ;; Don't prompt confirmation on writing backup, auto-save, or lock
  ;; file for a root-owned remote file
  (tramp-allow-unsafe-temporary-files t))

;;; find-func: find the definition of the Emacs Lisp function near point

(use-package find-func
  :bind
  (("C-h l" . find-library)))

;;; ffap: find file (or url) at point

(use-package ffap
  :custom
  (ffap-machine-p-known 'reject "Disallow pinging a host for a symbol that looks like a host"))

;;; Dired

(use-package dired
  :custom
  (dired-listing-switches "-l --almost-all --group-directories-first --human-readable")

  :config
  ;; Allow opening file, replacing current buffer
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Prerequisite on macOS: `brew install coreutils'
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")))

;;; Dirvish: replace Dired
;;;
;;; `https://github.com/alexluigit/dirvish'

(use-package dirvish
  :ensure t

  :demand

  :custom
  (dirvish-attributes '(vc-state
                        subtree-state
                        all-the-icons
                        collapse
                        git-msg
                        file-time
                        file-size))

  (dirvish-quick-access-entries
   '(("d" "~/Downloads/"               "Downloads")
     ("h" "~/Dropbox/Documents/howtos" "howtos")
     ("p" "~/Projects/"                "Projects")
     ("s" "~/Dropbox/Scratches/"       "Scratches")))

  :config
  (dirvish-override-dired-mode)

  :bind
  (("C-x C-d" . dirvish-dwim)
   ("C-c d"   . dirvish-fd)
   :map dirvish-mode-map
   ("TAB" . dirvish-subtree-toggle)
   ("^"   . dirvish-history-last)
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("h"   . dirvish-history-jump)
   ("s"   . dirvish-quicksort)
   ("y"   . dirvish-yank-menu)
   ("M-b" . dirvish-history-go-backward)
   ("M-f" . dirvish-history-go-forward)
   ("M-j" . dirvish-fd-jump)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle))
  )

;;; Uniquify: append dir name to buffers with similar filenames

(use-package uniquify
  :demand

  :custom
  (uniquify-buffer-name-style 'forward))

;;; Saveplace: save point location in the buffer when revisiting the
;;; buffer

(use-package saveplace
  :demand

  :custom
  (save-place-file (tk-init/user-emacs-path "saveplace"))

  :config
  (save-place-mode 1))

;;; Savehist: save minibuffer history

(use-package savehist
  :demand

  :custom
  (savehist-file (tk-init/user-emacs-path "savehist"))

  :config
  (savehist-mode 1))

;;; Recentf: shows list of recently opened files

(use-package recentf
  :demand

  :init
  (defun tk-editing/recentf-save-list-silent ()
    "Save the list of recent files periodically. Normally, recentf saves
the list when Emacs exits cleanly. If Emacs crashes, that save is
probably not done."
    (let ((inhibit-message t))
      (recentf-save-list)))

  :custom
  (recentf-save-file (tk-init/user-emacs-path "recentf"))

  (recentf-exclude
   (list
    (concat "\\`" (tk-init/user-emacs-path "recentf") "\\'")
    (concat "\\`" (tk-init/user-emacs-path "elpa") "/.*-autoloads.elc?\\'"))
   "Exclude recentf save file and Emacs ELPA autoloads")

  :config
  (run-at-time (* 5 60) (* 5 60) #'tk-editing/recentf-save-list-silent)

  (recentf-mode 1))

;;; Hippie-expand

(use-package hippie-exp
  :bind
  (("s-SPC" . hippie-expand)))

;;; Dabbrev

(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)

  :bind
  (("C-S-<tab>" . dabbrev-completion)))

(bind-key "C-<tab>" #'completion-at-point)

;;; Corfu
;;;
;;; `https://github.com/minad/corfu'

(use-package corfu
  :ensure t

  :demand

  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto popup")
  (corfu-auto-delay 0.5)
  (corfu-preselect 'prompt "Always preselect the prompt")
  (corfu-quit-no-match 'separator "Automatically quit there are no matching candidates, except when inserting `corfu-separator'")
  (corfu-on-exact-match 'insert "Insert sole candidate on exact match")

  :config
  (global-corfu-mode 1)

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("C-SPC" . corfu-insert-separator)))

;;; Vundo
;;;
;;; `https://github.com/casouri/vundo'

(use-package vundo
  :ensure t

  :custom
  (vundo-glyph-alist vundo-unicode-symbols)

  :bind
  (("C-x u" . vundo)))

;;; Projectile

(use-package projectile
  :ensure t

  :demand

  :custom
  (projectile-completion-system 'default)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-current-project-on-switch 'keep)

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

  (projectile-mode 1)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :bind
  (("C-c D" . projectile-dired)
   ("C-c O" . projectile-find-other-file)
   ("C-c f" . projectile-find-file-dwim)
   ("C-c o" . projectile-toggle-between-implementation-and-test)))

;;; Orderless: completion style that enables space-separated input
;;; components
;;;
;;; `https://github.com/oantolin/orderless'

(use-package orderless
  :ensure t

  :demand

  :custom
  ;; Set the `basic' completion style as fallback in order to ensure
  ;; that completion commands which rely on dynamic completion tables,
  ;; such as `completion-table-dynamic' or `completion-table-in-turn',
  ;; work correctly
  (completion-styles '(orderless basic))

  ;; Try the `basic' completion style first in order to make completion
  ;; work with Tramp
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Marginalia: add annotations to minibuffer completions
;;;
;;; `https://github.com/minad/marginalia'

(use-package marginalia
  :ensure t

  :demand

  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))

  :config
  (marginalia-mode 1))

;;; Vertico: minibuffer completion
;;;
;;; `https://github.com/minad/vertico'

(use-package vertico
  :ensure t

  :demand

  :config
  (vertico-mode 1)

  :after
  (orderless))

;;; Consult: search and navigation commands
;;;
;;; `https://github.com/minad/consult'

(use-package consult
  :ensure t

  :bind
  (
   ;; Editing
   ("M-y" . consult-yank-pop)
   ;; Global navigation
   ("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("C-x r b" . consult-bookmark)
   ("C-x r l" . consult-register-load)
   ("C-x r s" . consult-register-store)
   ("C-M-r"   . consult-register)
   ("C-c h"   . consult-history)
   ("C-c m"   . consult-man)
   ("C-c i"   . consult-info)
   ([remap Info-search] . consult-info)
   ;; Buffer local navigation
   ("M-g e"   . consult-compile-error)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("s-."     . consult-imenu)
   ("C-s-."   . consult-imenu-multi)
   ;; Searching
   ("M-s d" . consult-find)
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("C-c s" . consult-ripgrep)
   ("C-S-s" . consult-line)
   ("M-s L" . consult-line-multi)
   ;; Misc
   ("C-c M-x" . consult-mode-command)
   ;; Minibuffer key map for ordinary input (no completion). For
   ;; example, used in `M-:' (`eval-expression'). See:
   ;; `https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-Maps.html'
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)
   )

  :custom
  (consult-locate-args locate-command)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-function (lambda (_) (projectile-project-root)))

  :after
  (projectile)
  )

;;; Deadgrep: interface for ripgrep
;;;
;;; `https://github.com/Wilfred/deadgrep'

(use-package deadgrep
  :ensure t

  :init
  (defun tk-editing/deadgrep-show-result-other-window ()
    "Show the result in another window at point, keeping the
current search result window."
    (interactive)
    (let ((buf (car-safe (deadgrep--buffers))))
      (deadgrep-visit-result-other-window)
      (when buf
        (pop-to-buffer buf))))

  :custom
  ;; Projectile determines project root
  (deadgrep-project-root-function #'projectile-project-root)

  :bind
  (("C-c a" . deadgrep)
   :map deadgrep-mode-map
   ("C-c C-f" . next-error-follow-minor-mode)
   ("C-o"     . tk-editing/deadgrep-show-result-other-window)  ; Same as `compilation-display-error'
   ))

;;; Symbol-overlay
;;;
;;; `https://github.com/wolray/symbol-overlay'

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
    (bind-keys :map map
               ("M-N" . symbol-overlay-switch-forward)
               ("M-P" . symbol-overlay-switch-backward)
               ("M-e" . symbol-overlay-echo-mark)
               ("M-n" . symbol-overlay-jump-next)
               ("M-p" . symbol-overlay-jump-prev)
               ("M-q" . symbol-overlay-query-replace)
               ("M-r" . symbol-overlay-rename)
               ("M-s" . symbol-overlay-isearch-literally)
               ("M-t" . symbol-overlay-toggle-in-scope)
               ("M-w" . symbol-overlay-save-symbol))
    (setq symbol-overlay-map map))

  :bind
  (("s-O" . symbol-overlay-remove-all)
   ("s-o" . symbol-overlay-put)))

;;; Olivetti: minor-mode for writing text with comfortable horizontal
;;; margins
;;;
;;; `https://github.com/rnkn/olivetti'

(use-package olivetti
  :ensure t

  :custom
  (olivetti-style nil "Use margins to balance text")

  :config
  (add-to-list 'tk-looks/minor-mode-alist
               '(olivetti-mode " Olv"))

  :bind
  (("C-c V" . olivetti-mode)))
