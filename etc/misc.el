;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")

;; Dired: allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; IDO: Flexible matching (try exact match first, then fuzzy)
(setq ido-enable-flex-matching t)

;; IDO: disable ido-ubiquitous (would give the default value only for
;; certain prompts, such as when locating TAGS file)
(setq ido-ubiquitous-enabled nil)

;; Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (my-dotfile-path "ac-dictionary"))
(ac-config-default)

;; Enable UndoTree globally
(global-undo-tree-mode)

;; Enable search highlighting for ag
(setq ag-highlight-search t)

;; Enable auto pairing of brackets and quotation marks
(electric-pair-mode 1)

;; Hard wrapping at column number
(set-fill-column 78)

;; Default indentation
(setq standard-indent 2)

;; Do not insert tabs in place of multiple spaces when formatting a region
(setq-default indent-tabs-mode nil)

;; Typing text replaces active selection
(delete-selection-mode t)

(add-to-list 'safe-local-variable-values '(encoding . utf-8))
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Server mode
(server-start)
