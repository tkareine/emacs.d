(require 'package)

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
    counsel
    counsel-projectile
    dash-at-point
    enh-ruby-mode
    expand-region
    flycheck
    git-gutter
    highlight-symbol
    ivy
    js2-mode
    magit
    markdown-mode
    paredit
    projectile
    rjsx-mode
    scss-mode
    sql-indent
    swiper
    textile-mode
    undo-tree
    web-mode
    which-key
    yaml-mode
    zenburn-theme)
  "Minimum set of packages required for my configuration.")

;; Remove GNU Elpa package package archive, because the archive signature is invalid
;; <https://lists.gnu.org/archive/html/bug-gnu-emacs/2014-12/msg00781.html>
(customize-set-variable 'package-archives '(("melpa"        . "https://melpa.org/packages/")
                                            ("melpa-stable" . "https://stable.melpa.org/packages/")))

(customize-set-variable 'package-pinned-packages '((cider . "melpa-stable")))
