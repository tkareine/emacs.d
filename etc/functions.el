(defun tkareine/add-dotfile-to-load-path (p)
  (add-to-list 'load-path (tkareine/dotfile-path p)))

(defun tkareine/join-line ()
  (interactive)
  (join-line -1))

(defun tkareine/next-line-5 ()
  (interactive)
  (ignore-errors (next-line 5)))

(defun tkareine/previous-line-5 ()
  (interactive)
  (ignore-errors (previous-line 5)))

(defun tkareine/forward-char-5 ()
  (interactive)
  (ignore-errors (forward-char 5)))

(defun tkareine/backward-char-5 ()
  (interactive)
  (ignore-errors (backward-char 5)))

(defun tkareine/eol-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun tkareine/filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun tkareine/install-packages (packages)
  (message "Refreshing packages from repositories...")
  (package-refresh-contents)
  (message "Done refreshing packages from repositories.")
  (dolist (p packages) (package-install p)))

(defun tkareine/require-packages-installed (packages)
  (let* ((missing-packages (tkareine/filter '(lambda (x) (not (package-installed-p x))) packages)))
    (when missing-packages
      (message "Missing required packages, attempting to install them: %s" missing-packages)
      (tkareine/install-packages missing-packages))))

(defun tkareine/file-path-to-clipboard ()
  "Copy the current file name to the clipboard."
  (interactive)
  (let ((path (expand-file-name (or (buffer-file-name) default-directory))))
    (when path
      (let ((x-select-enable-clipboard t)) (x-select-text path))
      (kill-new path)
      (message path))))

(defun tkareine/js2-mode-toggle-strict-missing-semi-warning ()
  (interactive)
  (setq js2-strict-missing-semi-warning (eq js2-strict-missing-semi-warning nil))
  (js2-mode))

(defun tkareine/active-region-or-line ()
  (if mark-active (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (line-beginning-position 2))))

(defun tkareine/comment-or-uncomment-region-or-line ()
  (interactive)
  (let ((region (tkareine/active-region-or-line)))
    (when region
      (let ((rbegin (car region))
            (rend (cadr region)))
        (comment-or-uncomment-region rbegin rend)))))

(defun tkareine/pretty-print-xml (begin end)
  "Pretty format XML markup in region with nxml-mode."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (let* ((begin (or begin (point-min)))
         (end (or end (point-max)))
         (last-buf (current-buffer))
         (tmp-buf (generate-new-buffer (generate-new-buffer-name "*tkareine-pretty-print-xml-region*"))))
    (unwind-protect
        (save-excursion
          (with-current-buffer tmp-buf
            (insert-buffer-substring-no-properties last-buf begin end)
            (nxml-mode)
            (goto-char (point-min))
            ;; split <foo><foo> or </foo><foo>, but not <foo></foo>
            (while (search-forward-regexp ">[ \t]*<[^/]" end t)
              (backward-char 2)
              (insert "\n"))
            ;; split <foo/></foo> and </foo></foo>
            (goto-char (point-min))
            (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
              (backward-char)
              (insert "\n"))
            (indent-region (point-min) (point-max) nil)
            (delete-trailing-whitespace (point-min) (point-max)))
          (delete-region begin end)
          (insert-buffer-substring tmp-buf))
    (kill-buffer tmp-buf))))

(defun tkareine/toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (eq show-trailing-whitespace nil)))
