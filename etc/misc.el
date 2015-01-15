;; Default major-mode
(setq major-mode 'text-mode)

;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Hard wrapping at column number
(set-fill-column 78)

;; Default indentation
(setq standard-indent 2)

;; Add missing newline to file automatically when saving
(setq require-final-newline t)

;; Do not insert tabs in place of multiple spaces when formatting a region
(setq-default indent-tabs-mode nil)

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

;; Workaround for bug #12183: 24.1.50; Unrecognized pasteboard formats quit yank in Emacs.app
;; <http://emacs.1067599.n5.nabble.com/bug-12183-24-1-50-Unrecognized-pasteboard-formats-quit-yank-in-Emacs-app-td261226.html>
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Save clipboard strings into kill ring before replacing them
(setq save-interprogram-paste-before-kill t)

;; Apropos commands perform more extensive searches than default
(setq apropos-do-all t)

;; Mouse yanking inserts at the point instead of the location of the click
(setq mouse-yank-at-point t)

(add-to-list 'safe-local-variable-values '(encoding . utf-8))
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Server mode
(server-start)
