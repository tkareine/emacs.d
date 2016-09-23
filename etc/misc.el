;; Default major-mode
(customize-set-variable 'major-mode 'text-mode)

;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Hard wrapping at column number
(set-fill-column 78)

;; Default indentation
(customize-set-variable 'standard-indent 2)

;; Add missing newline to file automatically when saving
(customize-set-variable 'require-final-newline t)

;; Do not insert tabs in place of multiple spaces when formatting a region
(customize-set-variable 'indent-tabs-mode nil)

;; Allow downcase-region (C-x C-l), upcase-region (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; OS X: Use `mdfind` for locate
(if (eq system-type 'darwin)
    (setq locate-command "mdfind"))

;; Save typing chars when answering yes-or-no-p questions
(defalias 'yes-or-no-p 'y-or-n-p)

(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (tkareine/active-region-or-line)))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive (tkareine/active-region-or-line)))

;; Save clipboard strings into kill ring before replacing them
(customize-set-variable 'save-interprogram-paste-before-kill t)

;; Apropos commands perform more extensive searches than default
(setq apropos-do-all t)

;; Mouse yanking inserts at the point instead of the location of the click
(customize-set-variable 'mouse-yank-at-point t)

;; Network security
(customize-set-variable 'network-security-level 'high)

;; Safe buffer-local variables
(add-to-list 'safe-local-variable-values '(encoding . utf-8))
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; OS X: allow entering special chars via Option key
(customize-set-variable 'mac-option-modifier nil)

;; OS X: use Cmd as Meta modifier
(customize-set-variable 'mac-command-modifier 'meta)

;; Bind Ns key to Super modifer
(customize-set-variable 'ns-function-modifier 'super)

;; Other custom key bindings
(global-set-key (kbd "C-'")            #'text-scale-increase)
(global-set-key (kbd "C-;")            #'text-scale-decrease)
(global-set-key (kbd "C-B")            #'tkareine/backward-char-5)
(global-set-key (kbd "C-F")            #'tkareine/forward-char-5)
(global-set-key (kbd "C-M-r")          #'isearch-backward)
(global-set-key (kbd "C-M-s")          #'isearch-forward)
(global-set-key (kbd "C-N")            #'tkareine/next-line-5)
(global-set-key (kbd "C-P")            #'tkareine/previous-line-5)
(global-set-key (kbd "C-c B")          #'browse-url-at-point)
(global-set-key (kbd "C-c C")          #'comment-dwim)
(global-set-key (kbd "C-c C-c M-x")    #'execute-extended-command)
(global-set-key (kbd "C-c F")          #'find-file-at-point)
(global-set-key (kbd "C-c P")          #'tkareine/file-path-to-clipboard)
(global-set-key (kbd "C-r")            #'isearch-backward-regexp)
(global-set-key (kbd "C-s")            #'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b")        #'ibuffer)
(global-set-key (kbd "C-x W")          #'tkareine/toggle-show-trailing-whitespace)
(global-set-key (kbd "C-x t")          #'delete-trailing-whitespace)
(global-set-key (kbd "C-x w")          #'whitespace-mode)
(global-set-key (kbd "M-/")            #'tkareine/comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-<kp-delete>")  #'kill-word)
(global-set-key (kbd "M-J")            #'delete-indentation)
(global-set-key (kbd "M-S-SPC")        #'fixup-whitespace)
(global-set-key (kbd "M-[")            #'point-to-register)
(global-set-key (kbd "M-]")            #'jump-to-register)
(global-set-key (kbd "M-j")            #'tkareine/join-line)
(global-set-key (kbd "M-§")            #'other-frame)
(global-set-key (kbd "S-<down>")       #'windmove-down)
(global-set-key (kbd "S-<left>")       #'windmove-left)
(global-set-key (kbd "S-<return>")     #'tkareine/eol-newline-and-indent)
(global-set-key (kbd "S-<right>")      #'windmove-right)
(global-set-key (kbd "S-<up>")         #'windmove-up)
(global-set-key (kbd "s-SPC")          #'hippie-expand)

;; Server mode
(server-start)
