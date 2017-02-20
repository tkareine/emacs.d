;; OS X: Use `mdfind` for locate
(if (eq system-type 'darwin)
    (customize-set-variable 'locate-command "mdfind"))

;; Apropos commands perform more extensive searches than default
(customize-set-variable 'apropos-do-all t)

;; Safe buffer-local variables
(add-to-list 'safe-local-variable-values '(encoding . utf-8))
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))
