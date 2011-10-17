;; Prefer UTF-8 encoding for input and output
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Dired: allow opening file, replacing current buffer
(put 'dired-find-alternate-file 'disabled nil)

;; IDO: Flexible matching (try exact match first, then fuzzy)
(setq ido-enable-flex-matching t)

;; IDO: disable ido-ubiquitous (would give the default value only for
;; certain prompts, such as when locating TAGS file)
(setq ido-ubiquitous-enabled nil)

;; Hard wrapping at column number
(set-fill-column 72)

;; Default indentation
(setq standard-indent 2)

;; Markdown file types
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(defun my-file-path-to-clipboard ()
  "Copy the current file name to the clipboard"
  (interactive)
  (let ((path (expand-file-name (or (buffer-file-name) default-directory))))
    (when path
      (let ((x-select-enable-clipboard t)) (x-select-text path))
      (message path))))

;; Server mode
(server-start)
