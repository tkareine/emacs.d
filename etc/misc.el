;; Prefer UTF-8 encoding for input and output
(set-language-environment "UTF-8")

;; Dired: allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; IDO: Enable globally
(ido-mode t)

;; IDO: Flexible matching (try exact match first, then fuzzy)
(setq ido-enable-flex-matching t)

;; Append dir name to buffers with similar filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save point location in the buffer when revisiting the buffer
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (my-dotfile-path "places"))

;; Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (my-dotfile-path "ac-dictionary"))
(ac-config-default)

;; Enable UndoTree globally
(global-undo-tree-mode)

;; Tramp: Prefer ssh
(setq tramp-default-method "ssh")

;; Ag: enable search highlighting
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
