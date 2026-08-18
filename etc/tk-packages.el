;; -*- lexical-binding: t; -*-

;;; Straight.el
;;;
;;; Docs: `https://github.com/radian-software/straight.el/blob/main/README.md'

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
;;
;; Docs: `https://jwiegley.github.io/use-package/'
(setq-default use-package-enable-imenu-support t)

(use-package transient
  :straight t)

(use-package tk-support
  :commands
  (tk-support/active-region-or-line
   tk-support/locate-any-dominating-file
   tk-support/string-prefix-length-with-char
   tk-support/xml-pretty-print)

  :load-path
  "site-lisp/tk-support")
