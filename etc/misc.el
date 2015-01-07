;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Hard wrapping at column number
(set-fill-column 78)

;; Default indentation
(setq standard-indent 2)

;; Do not insert tabs in place of multiple spaces when formatting a region
(setq-default indent-tabs-mode nil)

;; Save typing chars when answering yes-or-no-p questions
(defalias 'yes-or-no-p 'y-or-n-p)

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
