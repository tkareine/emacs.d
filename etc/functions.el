(defun my-join-line ()
  (interactive)
  (join-line -1))

(defun my-next-line-5 ()
  (interactive)
  (ignore-errors (next-line 5)))

(defun my-previous-line-5 ()
  (interactive)
  (ignore-errors (previous-line 5)))

(defun my-forward-char-5 ()
  (interactive)
  (ignore-errors (forward-char 5)))

(defun my-backward-char-5 ()
  (interactive)
  (ignore-errors (backward-char 5)))

(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun my-install-packages (packages)
  (message "Refreshing packages from repositories...")
  (package-refresh-contents)
  (message "Done refreshing packages from repositories.")
  (dolist (p packages) (package-install p)))

(defun my-require-packages-installed (packages)
  (let* ((missing-packages (my-filter '(lambda (x) (not (package-installed-p x))) packages)))
    (when missing-packages
      (message "Missing required packages, attempting to install them: %s" missing-packages)
      (my-install-packages missing-packages))))

(defun my-file-path-to-clipboard ()
  "Copy the current file name to the clipboard"
  (interactive)
  (let ((path (expand-file-name (or (buffer-file-name) default-directory))))
    (when path
      (let ((x-select-enable-clipboard t)) (x-select-text path))
      (kill-new path)
      (message path))))

(defun my-js2-mode-toggle-strict-missing-semi-warning ()
  (interactive)
  (setq js2-strict-missing-semi-warning (eq js2-strict-missing-semi-warning nil))
  (js2-mode))

(defun my-open-path-at-point ()
  "Open the URL or file path at point.
If region is active, uses its content for path. If the path
starts with \"http(s)://\" or \"mailto:\", open the URL in
browser. Input path can be {relative, full path, URL}. This
command is similar to `find-file-at-point' but without prompting
for confirmation.
"
  (interactive)
  (let ((path (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'filename))))
    (if (string-match-p "\\`\\(https?://\\)\\|\\(mailto:\\)" path)
        (browse-url path)
      (progn
        (if (file-exists-p path)
            (find-file path)
          (if (file-exists-p (concat path ".el"))
              (find-file (concat path ".el"))
            (when (y-or-n-p (format "File doesn't exist: %s. Create?" path) )
              (find-file path))))))))
