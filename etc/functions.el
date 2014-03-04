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

(defun tkareine/open-url-at-point ()
  "Open the URL at point in browser.
If region is active, uses its content for URL, otherwise use the
URL at point.
"
  (interactive)
  (let ((url (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'url))))
    (browse-url url)))

(defun tkareine/active-region-or-line ()
  (if mark-active (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (line-beginning-position 2))))
