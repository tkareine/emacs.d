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
