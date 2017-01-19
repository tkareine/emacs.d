;; Disable visible bell, is broken on OS X El Capitan
(customize-set-variable 'visible-bell nil)

;; Replace ring-bell with my own function
(customize-set-variable 'ring-bell-function #'tkareine/visible-bell)

;; Syntax higlighting where applicable
(global-font-lock-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Standard selection-highlighting behavior of other edit
(transient-mark-mode t)

;; See matching pairs of parentheses and other characters
(show-paren-mode t)

;; Show current line and column in the mode line
(line-number-mode t)
(column-number-mode t)

;; Don't show line number if buffer is too big; value in bytes
(customize-set-variable 'line-number-display-limit (* 1024 1024 64))

;; Show buffer size in the mode line
(size-indication-mode 1)

;; No blinking cursor
(blink-cursor-mode 0)

;; Highlight long lines when whitespace-mode is enabled
(customize-set-variable 'whitespace-line-column 140)

;; Highlight trailing whitespaces in lines
(customize-set-variable 'show-trailing-whitespace t)

;; Show file size
(size-indication-mode t)

;; Font
;; (set-face-font 'default "Inconsolata-16")
(set-face-font 'default "Input-14")
(customize-set-variable 'line-spacing 2)

;; Shorter position, e.g.: `27%/3.0k`. Adapted from
;; <http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html>.
(customize-set-variable 'mode-line-position
                        '((-3 "%p")
                          (size-indication-mode ("/" (-4 "%I")))
                          " "
                          (line-number-mode ("%l" (column-number-mode ":%c")))))

(defvar tkareine/mode-line-projectile-project
  '(:eval (when (ignore-errors (projectile-project-root))
            (let ((project-name (projectile-project-name)))
              (put-text-property 0
                                 (length project-name)
                                 'face
                                 'font-lock-constant-face
                                 project-name)
              (concat "  " project-name))))
  "Mode line construct for project name from Projectile.")

(put 'tkareine/mode-line-projectile-project 'risky-local-variable t)

(defvar tkareine/mode-line-vc
  '(vc-mode (" "
             (:propertize vc-mode
                          face
                          font-lock-variable-name-face)))
  "Mode line construct for VC mode.")

(put 'tkareine/mode-line-vc 'risky-local-variable t)

(defvar tkareine/mode-line-flycheck
  '(flycheck-mode (" "
                   (11 "" flycheck-mode-line)))
  "Mode line construct for Flycheck.")

(put 'tkareine/mode-line-flycheck 'risky-local-variable t)

(defvar tkareine/mode-line-modes
  '(#("%[" 0 2 (face font-lock-warning-face help-echo "Recursive edit, type C-M-c to cancel"))
    "("
    mode-name
    ("" mode-line-process)
    #("%n" 0 2 (face font-lock-warning-face help-echo "Narrowing, type C-x n w to cancel"))
    ")"
    #("%]" 0 2 (face font-lock-warning-face help-echo "Recursive edit, type C-M-c to cancel"))
    " "
    (server-buffer-clients " Server")
    (paredit-mode paredit-lighter)
    (defining-kbd-macro " Def")
    (diff-minor-mode " Diff")
    (global-whitespace-newline-mode " NL")
    (global-whitespace-mode " WS")
    (whitespace-newline-mode " nl")
    (whitespace-mode " ws")
    (visible-mode " Vis")
    (visual-line-mode " Wrap")
    (superword-mode " ²")
    (edebug-mode " *Debugging*")
    (compilation-minor-mode " Compilation")
    (compilation-in-progress " Compiling")
    (auto-revert-tail-mode auto-revert-tail-mode-text)
    (auto-revert-mode auto-revert-mode-text)
    (eldoc-mode eldoc-minor-mode-string)
    (next-error-follow-minor-mode " Fol")
    (abbrev-mode " Abbrev")
    (overwrite-mode overwrite-mode)
    (auto-fill-function " Fill"))
  "Mode line construct for major mode (with recursive edit,
  mode-line-process, and narrowing) and selected minor modes.")

(put 'tkareine/mode-line-modes 'risky-local-variable t)

(customize-set-variable 'mode-line-format
                        '("%e"
                          mode-line-front-space
                          mode-line-mule-info
                          mode-line-client
                          mode-line-modified
                          mode-line-remote
                          mode-line-frame-identification
                          mode-line-buffer-identification
                          "  "
                          mode-line-position
                          tkareine/mode-line-projectile-project
                          tkareine/mode-line-vc
                          tkareine/mode-line-flycheck
                          " "
                          mode-line-misc-info
                          " "
                          tkareine/mode-line-modes
                          mode-line-end-spaces))

;; Frame title: show
;; 1. hostname
;; 2. buffer file name, dired directory, or buffer name
;; 3. buffer modification marker
(customize-set-variable 'frame-title-format
                        `(,(system-name)
                          ": "
                          (buffer-file-name "%f" (dired-directory dired-directory "%b"))
                          " %*"
                          (multiple-frames (:eval (concat " [" (number-to-string (length (frame-list))) "]")))))

;; Frame width and height
(if (display-graphic-p) (set-frame-size (selected-frame) 140 60))

;; Color theme
;;(require 'color-theme-my-twilight)
;;(color-theme-my-twilight)
(load-theme 'zenburn t)
