;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Hard wrapping at column number
(customize-set-variable 'fill-column 72)

;; Default indentation
(customize-set-variable 'standard-indent 2)

;; Add missing newline to file automatically when saving
(customize-set-variable 'require-final-newline t)

;; Do not insert tabs in place of multiple spaces when formatting a region
(customize-set-variable 'indent-tabs-mode nil)

;; Allow downcase-region (C-x C-l), upcase-region (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; OS X: allow entering special chars via Option key
(customize-set-variable 'mac-option-modifier nil)

;; OS X: use Cmd as Meta modifier
(customize-set-variable 'mac-command-modifier 'meta)

;; Bind Ns key to Super modifer
(customize-set-variable 'ns-function-modifier 'super)

;; Save typing chars when answering yes-or-no-p questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Navigation

(defun tkareine/join-line ()
  (interactive)
  (join-line -1))

(global-set-key (kbd "M-j") #'tkareine/join-line)

(defun tkareine/next-line-5 ()
  (interactive)
  (ignore-errors (forward-line 5)))

(global-set-key (kbd "C-n") #'tkareine/next-line-5)

(defun tkareine/previous-line-5 ()
  (interactive)
  (ignore-errors (forward-line -5)))

(global-set-key (kbd "C-p") #'tkareine/previous-line-5)

(defun tkareine/forward-char-5 ()
  (interactive)
  (ignore-errors (forward-char 5)))

(global-set-key (kbd "C-f") #'tkareine/forward-char-5)

(defun tkareine/backward-char-5 ()
  (interactive)
  (ignore-errors (backward-char 5)))

(global-set-key (kbd "C-b") #'tkareine/backward-char-5)

(defun tkareine/eol-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") #'tkareine/eol-newline-and-indent)

(defun tkareine/file-path-to-clipboard ()
  "Copy the current file name to the clipboard."
  (interactive)
  (let ((path (expand-file-name (or (buffer-file-name) default-directory))))
    (when path
      (let ((select-enable-clipboard t)) (gui-select-text path))
      (kill-new path)
      (message path))))

(global-set-key (kbd "C-c P") #'tkareine/file-path-to-clipboard)

(global-set-key (kbd "C-c F")     #'find-file-at-point)
(global-set-key (kbd "S-<down>")  #'windmove-down)
(global-set-key (kbd "S-<left>")  #'windmove-left)
(global-set-key (kbd "S-<right>") #'windmove-right)
(global-set-key (kbd "S-<up>")    #'windmove-up)
(global-set-key (kbd "M-ESC")     #'other-frame)
(global-set-key (kbd "M-§")       #'other-frame)

;; Force learning to avoid using M-<left|right> for movement between words
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))

;; Kill ring

(defun tkareine/kill-ring-save-advice (adviced &rest arguments)
  "When called interactively with no active region, copy a single line instead."
  (interactive (tkareine/active-region-or-line))
  (apply adviced arguments))
(advice-add #'kill-ring-save :around #'tkareine/kill-ring-save-advice)

(defun tkareine/kill-region-advice (adviced &rest arguments)
  "When called interactively with no active region, kill a single line instead."
  (interactive (tkareine/active-region-or-line))
  (apply adviced arguments))
(advice-add #'kill-region :around #'tkareine/kill-region-advice)

(global-set-key (kbd "M-<kp-delete>") #'kill-word)

;; Save clipboard strings into kill ring before replacing them
(customize-set-variable 'save-interprogram-paste-before-kill t)

;; Mouse yanking inserts at the point instead of the location of the click
(customize-set-variable 'mouse-yank-at-point t)

;; Commenting

(defun tkareine/comment-or-uncomment-region-or-line ()
  (interactive)
  (let ((region (tkareine/active-region-or-line)))
    (when region
      (let ((rbegin (car region))
            (rend (cadr region)))
        (comment-or-uncomment-region rbegin rend)))))

(global-set-key (kbd "M-/")   #'tkareine/comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C") #'comment-dwim)

;; Whitespace

(defun tkareine/toggle-show-trailing-whitespace ()
  (interactive)
  (customize-set-variable 'show-trailing-whitespace (eq show-trailing-whitespace nil)))

(global-set-key (kbd "C-x W")   #'tkareine/toggle-show-trailing-whitespace)

(global-set-key (kbd "C-x t")   #'delete-trailing-whitespace)
(global-set-key (kbd "C-x w")   #'whitespace-mode)
(global-set-key (kbd "M-S-SPC") #'fixup-whitespace)
(global-set-key (kbd "M-J")     #'delete-indentation)

;; Registers
(global-set-key (kbd "M-[") #'point-to-register)
(global-set-key (kbd "M-]") #'jump-to-register)

;; Other
(global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)
(global-set-key (kbd "C-x C-b")     #'ibuffer)
(global-set-key (kbd "C-c U")       #'browse-url-at-point)
