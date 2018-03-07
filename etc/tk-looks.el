;; Hide tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Hide scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Do not show splash screen
(customize-set-variable 'inhibit-startup-message t)

;; Do not show startup message
(customize-set-variable 'inhibit-startup-echo-area-message (user-login-name))

;; Set no content in *scratch* buffer
(customize-set-variable 'initial-scratch-message "")

;; Disable visible bell, is broken on OS X El Capitan
(customize-set-variable 'visible-bell nil)

(defun tk-looks/visible-bell ()
  "Replace ring-bell. Adapted from URL
`https://www.emacswiki.org/emacs/AlarmBell'."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(customize-set-variable 'ring-bell-function #'tk-looks/visible-bell)

;; Syntax higlighting where applicable
(global-font-lock-mode)

;; Highlight current line
(global-hl-line-mode)

;; Selection-highlighting behavior like in other editors
(transient-mark-mode)

;; See matching pairs of parentheses and other characters
(show-paren-mode)

;; No blinking cursor
(blink-cursor-mode -1)

;; Highlight long lines when whitespace-mode is enabled
(customize-set-variable 'whitespace-line-column 140)

;; Highlight trailing whitespaces in lines
(customize-set-variable 'show-trailing-whitespace t)

;; Color theme
(require 'zenburn-theme)
(load-theme 'zenburn t)

;;; Mode line

;; Show current line and column in mode line
(line-number-mode)
(column-number-mode)

;; Show buffer size in mode line
(size-indication-mode)

;; Don't show line number if buffer is too big; value in bytes
(customize-set-variable 'line-number-display-limit (* 1024 1024 64))

;; Show current function in mode line
(which-function-mode)

(defvar tk-looks/mode-line-position
  '((line-number-mode ("%l" (column-number-mode ":%c")))
    " "
    (-3 "%p")
    (size-indication-mode ("/" (-4 "%I"))))
  "Mode line construct for point position in the buffer. Example: `71:0 23%/7.7k'.

Adapted from URL
`http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html'.")

(put 'tk-looks/mode-line-position 'risky-local-variable t)

(require 'projectile)

(defvar tk-looks/mode-line-projectile-project
  '(:eval (when (ignore-errors (projectile-project-root))
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
    (global-whitespace-mode " WS")
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
    (auto-fill-function " Fill")
    (paredit-mode paredit-lighter)
    (ggtags-mode (:eval (if ggtags-navigation-mode " GG[nav]" " GG"))))
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
                          tk-looks/mode-line-position
                          tk-looks/mode-line-projectile-project
                          tk-looks/mode-line-vc
                          tk-looks/mode-line-flycheck
                          " "
                          mode-line-misc-info
                          " "
                          tk-looks/mode-line-modes
                          mode-line-end-spaces))

(zenburn-with-color-variables
  (custom-set-faces `(mode-line ((t (:box (:line-width 1
                                           :color ,zenburn-green-2)))))
                    `(mode-line-inactive ((t (:box (:line-width 1
                                                    :color ,zenburn-bg-2)))))))

;;; Frames

;; Frame title: show
;; 1. hostname
;; 2. buffer file name, dired directory, or buffer name
;; 3. buffer modification marker
;; 4. number of frames
(customize-set-variable 'frame-title-format
                        `(,(system-name)
                          ": "
                          (buffer-file-name "%f" (dired-directory dired-directory "%b"))
                          " %*"
                          (multiple-frames (:eval (concat " [" (number-to-string (length (frame-list))) "]")))))

;; Maximize initial frame
(customize-set-variable 'default-frame-alist
                        '((fullscreen . maximized)))

;;; Font

(set-face-font 'default "Input-14")

(customize-set-variable 'line-spacing 2)

(global-set-key (kbd "C-'") #'text-scale-increase)
(global-set-key (kbd "C-;") #'text-scale-decrease)

;;; GitGutter

(customize-set-variable 'git-gutter:lighter " gg")

(global-git-gutter-mode)

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

(global-set-key (kbd "s-O") #'symbol-overlay-remove-all)
(global-set-key (kbd "s-n") #'symbol-overlay-switch-forward)
(global-set-key (kbd "s-o") #'symbol-overlay-put)
(global-set-key (kbd "s-p") #'symbol-overlay-switch-backward)

;;; Which-key: show available key bindings

(which-key-mode)
