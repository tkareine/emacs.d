(defun tkareine/add-selected-packages (packages)
  (let ((new-packages (delete-dups (append packages
                                           package-selected-packages))))
    (customize-set-variable 'package-selected-packages new-packages)))

(defun tkareine/install-packages (packages)
  (message "Refreshing packages from repositories...")
  (package-refresh-contents)
  (message "Done refreshing packages from repositories.")
  (dolist (p packages)
    (message "Installing package: %s" p)
    (package-install p)))

(defun tkareine/install-missing-packages (packages)
  (let ((missing-packages (tkareine/filter (lambda (x) (not (package-installed-p x)))
                                           packages)))
    (when missing-packages
      (message "Installing missing packages: %s" missing-packages)
      (tkareine/install-packages missing-packages))))

(defvar tkareine/package-dependencies
  '(ag
    company
    dash-at-point
    enh-ruby-mode
    expand-region
    flycheck
    git-gutter
    helm
    helm-ag
    helm-projectile
    highlight-symbol
    js2-mode
    magit
    markdown-mode
    paredit
    projectile
    rjsx-mode
    scss-mode
    undo-tree
    yaml-mode
    web-mode
    which-key
    zenburn-theme)
  "Minimum set of packages required for my configuration")