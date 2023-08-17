;; -*- lexical-binding: t; -*-

;; Hide menu bar
(menu-bar-mode 0)

;; Hide tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Hide scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Do not show splash screen
(setq-default inhibit-startup-screen t)

;; A hack to prevent showing the startup message. See
;; `display-startup-echo-area-message' and
;; `https://lists.gnu.org/archive/html/bug-gnu-emacs/2012-12/msg00954.html'.
;;
;; Temporarily disable the hack, because the `put' call causes error
;; with `custom.el'. Steps to reproduce:
;;
;; 1. Enable the hack below
;; 2. Start Emacs
;; 3. Call the `list-packages' command
;; 4. Error in `custom-save-all': Wrong type argument: listp, t
;;
;; (put 'inhibit-startup-echo-area-message 'saved-value t)
;; (setq inhibit-startup-echo-area-message (user-login-name))

;; Set no content in *scratch* buffer
(setq-default initial-scratch-message "")

;; Disable visible bell, is broken on OS X El Capitan
(setq-default visible-bell nil)

(defun tk-looks/visible-bell ()
  "Replace ring-bell. Adapted from URL
`https://www.emacswiki.org/emacs/AlarmBell'."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq-default ring-bell-function #'tk-looks/visible-bell)

;; Syntax higlighting where applicable
(global-font-lock-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Selection-highlighting behavior like in other editors
(transient-mark-mode 1)

;; See matching pairs of parentheses and other characters
(show-paren-mode 1)

;; No blinking cursor
(blink-cursor-mode -1)

(use-package whitespace
  :custom
  ;; Highlight long lines when whitespace-mode is enabled
  (whitespace-line-column 120)

  ;; Highlight trailing whitespaces with the `trailing-whitespace' face;
  ;; use this instead of including `trailing' in the `whitespace-style'
  ;; list configuration of whitespace.el. That way, we see trailing
  ;; whitespace irrespective of whitespace.el.
  (show-trailing-whitespace t)

  (whitespace-style '(face
                      tabs
                      lines
                      ;; don't include `trailing', see comment above
                      space-after-tab::tab
                      space-before-tab::tab
                      tab-mark))

  :custom-face
  (whitespace-tab ((t (:background "grey30"))))
  (whitespace-line ((t (:background "#66494a" :foreground unspecified))))

  :config
  ;; Enable global whitespace mode
  (global-whitespace-mode 1))

;; Color theme
(use-package zenburn-theme
  :ensure t

  :demand

  :config
  (load-theme 'zenburn t)

  (zenburn-with-color-variables
    (custom-set-faces `(region ((,class (:background ,zenburn-bg-2))))
                      `(mode-line ((,class (:box (:line-width 1
                                                  :color ,zenburn-yellow-2)))))
                      `(mode-line-inactive ((,class (:box (:line-width 1
                                                           :color ,zenburn-bg-2)))))
                      `(next-error ((t (:background ,zenburn-red-2))))  ; used by `compilation-display-error'
                      `(highlight ((t (:background ,zenburn-blue-3))))  ; used by Ivy and deadgrep
                      )))

;;; Mode line

;; Show current line and column in mode line
(line-number-mode 1)
(column-number-mode 1)

;; Show buffer size in mode line
(size-indication-mode 1)

;; Show buffer boundaries with angle bitmaps and scrolling with arrow
;; bitmaps in the left fringe
(setq-default indicate-buffer-boundaries 'left)

;; Visually indicate empty lines in buffer in the left fringe
(setq-default indicate-empty-lines t)

;; Don't show line number if buffer is too big; value in bytes
(setq-default line-number-display-limit
              (let ((mb (* 1024 1024))) (* 100 mb)))

(use-package which-func
  :config
  ;; Show current function in mode line
  (which-function-mode 1))

(defvar tk-looks/mode-line-position
  '((line-number-mode ("%l" (column-number-mode ":%c")))
    " "
    (-3 "%p")
    (size-indication-mode ("/" (-4 "%I"))))
  "Mode line construct for point position in the buffer. Example: `71:0 23%/7.7k'.

Adapted from
`https://web.archive.org/web/20170129214645/http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html'.")

(put 'tk-looks/mode-line-position 'risky-local-variable t)

(defvar tk-looks/mode-line-projectile-project
  '(:eval (when (and
                 ;; Calling `projectile-project-root' is slow with Tramp
                 ;; buffer. Check if the current buffer is associated
                 ;; with Tramp (but avoid loading Tramp).
                 (not (ignore-errors (tramp-tramp-file-p (buffer-file-name (current-buffer)))))
                 (ignore-errors (projectile-project-root)))
            (let ((project-name (projectile-project-name)))
              (put-text-property 0
                                 (length project-name)
                                 'face
                                 'font-lock-constant-face
                                 project-name)
              (concat "  " project-name))))
  "Mode line construct for project name from Projectile.")

(put 'tk-looks/mode-line-projectile-project 'risky-local-variable t)

(defvar tk-looks/mode-line-vc
  '(vc-mode (" "
             (:propertize vc-mode
                          face
                          font-lock-variable-name-face)))
  "Mode line construct for VC mode.")

(put 'tk-looks/mode-line-vc 'risky-local-variable t)

(defvar tk-looks/mode-line-flycheck
  '(flycheck-mode (" "
                   (11 "" flycheck-mode-line)))
  "Mode line construct for Flycheck.")

(put 'tk-looks/mode-line-flycheck 'risky-local-variable t)

(defvar tk-looks/minor-mode-alist
  '((defining-kbd-macro " Def")
    (server-buffer-clients " Server")
    (overwrite-mode overwrite-mode)
    (global-whitespace-newline-mode " NL")
    (whitespace-newline-mode " nl")
    (whitespace-mode " ws")
    (visible-mode " Vis")
    (visual-line-mode " Wrap")
    (superword-mode " ²")
    (auto-revert-tail-mode auto-revert-tail-mode-text)
    (auto-revert-mode auto-revert-mode-text)
    (edebug-mode " *Debugging*")
    (compilation-minor-mode " Compilation")
    (compilation-in-progress " Compiling")
    (diff-minor-mode " Diff")
    (eldoc-mode eldoc-minor-mode-string)
    (next-error-follow-minor-mode " Fol")
    (abbrev-mode " Abbrev")
    (auto-fill-function " Fill"))
  "Alist of selected minor modes to be shown in mode line.")

(put 'tk-looks/minor-mode-alist 'risky-local-variable t)

(defvar tk-looks/mode-line-modes
  '(#("%[" 0 2 (face font-lock-warning-face help-echo "Recursive edit, type C-M-c to cancel"))
    "("
    mode-name
    mode-line-process
    #("%n" 0 2 (face font-lock-warning-face help-echo "Narrowing, type C-x n w to cancel"))
    ")"
    #("%]" 0 2 (face font-lock-warning-face help-echo "Recursive edit, type C-M-c to cancel"))
    " "
    tk-looks/minor-mode-alist)
  "Mode line construct for major mode (with recursive edit,
mode-line-process, and narrowing) and selected minor modes.")

(put 'tk-looks/mode-line-modes 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                tk-looks/mode-line-position
                tk-looks/mode-line-projectile-project
                tk-looks/mode-line-vc
                tk-looks/mode-line-flycheck
                " "
                mode-line-misc-info
                " "
                tk-looks/mode-line-modes
                mode-line-end-spaces))

;;; Frames

;; Frame title: show
;; 1. hostname
;; 2. buffer file name, dired directory, or buffer name
;; 3. buffer modification marker
;; 4. number of frames
(setq-default frame-title-format
              `(,(system-name)
                ": "
                (buffer-file-name "%f" (dired-directory dired-directory "%b"))
                " %*"
                (multiple-frames (:eval (concat " [" (number-to-string (length (frame-list))) "]")))))

;; Maximize initial frame
(setq-default default-frame-alist
              '((fullscreen . maximized)))

;;; Font
(let* ((selected-font "Input-14"))
  (set-face-attribute 'default nil :font selected-font)
  (set-face-attribute 'fixed-pitch nil :font selected-font))

(setq-default line-spacing 2)

(bind-keys ("C-:"  . text-scale-decrease)
           ("C-\"" . text-scale-increase))

(when (featurep 'mac-win)
  (bind-keys
   ;; macOS: remove changing text scaling or full screen mode with gestures
   ([S-magnify-down] . nil)
   ([S-magnify-up]   . nil)
   ([magnify-down]   . nil)
   ([magnify-up]     . nil)
   ;; macOS: don't toggle showing tab bar
   ([C-tab] . nil)))

;;; GitGutter

(use-package git-gutter
  :ensure t

  :demand

  :custom
  (git-gutter:lighter " gg")

  :config
  (global-git-gutter-mode 1))

;;; rainbow-delimiters

(use-package rainbow-delimiters
  :ensure t

  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Which-key: show available key bindings

(use-package which-key
  :ensure t

  :demand

  :config
  (which-key-mode 1))
