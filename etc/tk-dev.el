(require 'tk-support)

;;; Ediff

;; Use current frame for control panel
(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

;; I find it easier to read a diff by having one buffer on the left and
;; another on the right
(customize-set-variable 'ediff-split-window-function 'split-window-horizontally)

;;; Ggtags

(defun tk-dev/make-gtags (rootdir)
  "Make gtags files to the current project.

If called with a prefix, specify the directory to make gtags files for."
  (interactive (cl-flet ((read-dir ()
                                   (read-directory-name "Make GTAGS to: " nil nil t)))
                 (let ((dir (if current-prefix-arg
                                (read-dir)
                              (if-let ((proj-dir (projectile-project-root)))
                                  proj-dir
                                (read-dir)))))
                    (list dir))))
  (let ((current-prefix-arg nil)) ; reset as it might affect future commands
    (require 'ggtags)
    (ggtags-create-tags rootdir)))

(defun tk-dev/ggtags-mode-customizations ()
  (define-key ggtags-mode-map (kbd "M-]") nil)

  ;; don't change `mode-line-buffer-identification', because we
  ;; show project root dir in the mode line with projectile
  (setq ggtags-mode-line-project-name nil))

(eval-after-load 'ggtags #'tk-dev/ggtags-mode-customizations)

(defun tk-dev/ggtags-adjust-tag-bounds-for-scss-mode (org-bounds)
  "Adjusts tag bounds so that `$var' gets converted to `var'. The
dollar sign does not belong to SCSS variable symbol in our
configuration for GNU Global."
  (pcase org-bounds
    (`(,org-beg . ,org-end)
     (let* ((tag-str (buffer-substring org-beg org-end))
            (dollar-prefix-length (tk-support/string-prefix-length-with-char ?$ tag-str))
            (new-beg (+ org-beg dollar-prefix-length))
            (new-bounds (if (< new-beg org-end)
                            (cons new-beg org-end)
                          org-bounds)))
       new-bounds))))

(defun tk-dev/ggtags-bounds-of-tag ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (pcase major-mode
      ('scss-mode (tk-dev/ggtags-adjust-tag-bounds-for-scss-mode bounds))
      (- bounds))))

(customize-set-variable 'ggtags-bounds-of-tag-function #'tk-dev/ggtags-bounds-of-tag)

(customize-set-variable 'ggtags-process-environment '("GTAGSLABEL=default"))

(add-hook 'enh-ruby-mode-hook #'ggtags-mode)
(add-hook 'js2-mode-hook      #'ggtags-mode)
(add-hook 'less-css-mode-hook #'ggtags-mode)
(add-hook 'scss-mode-hook     #'ggtags-mode)

(global-set-key (kbd "C-c T") #'tk-dev/make-gtags)
(global-set-key (kbd "C-c r") #'ggtags-find-reference)
(global-set-key (kbd "C-c t") #'ggtags-find-tag-dwim)

;;; Company

(require 'company)

;; Don't lowercase completion candidates (dabbrev backend)
(customize-set-variable 'company-dabbrev-downcase nil)

;; Ignore case when collecting completion candidates and copy candidate
;; verbatim (dabbrev and dabbrev-code backends)
(customize-set-variable 'company-dabbrev-ignore-case t)
(customize-set-variable 'company-dabbrev-code-ignore-case t)

(global-company-mode)

;;; CSS

(customize-set-variable 'css-indent-offset 2)

;;; C family

(customize-set-variable 'c-basic-offset 4)

(customize-set-variable 'c-default-style '((awk-mode  . "awk")
                                           (java-mode . "java")
                                           (other     . "linux")))

;;; JavaScript

(customize-set-variable 'js-indent-level 2)
(customize-set-variable 'js2-basic-offset 2)
(customize-set-variable 'js2-bounce-indent-p t)
(customize-set-variable 'js2-concat-multiline-strings nil)
(customize-set-variable 'js2-highlight-level 3)
(customize-set-variable 'js2-missing-semi-one-line-override t)
(customize-set-variable 'js2-strict-missing-semi-warning nil)

;; Don't double-indent multiline statement
(advice-add #'js--multi-line-declaration-indentation
            :around
            (lambda (orig-fun &rest args)))

(custom-set-faces '(js2-private-member ((t (:foreground "coral1")))))

(defun tk-dev/js2-mode-toggle-strict-missing-semi-warning ()
  (interactive)
  (setq js2-strict-missing-semi-warning (eq js2-strict-missing-semi-warning nil))
  (js2-mode))

(defun tk-dev/js2-mode-customizations ()
  (define-key js2-mode-map (kbd "M-.")   nil)
  (define-key js2-mode-map (kbd "C-c j") #'tk-dev/js2-mode-toggle-strict-missing-semi-warning))

(eval-after-load 'js2-mode #'tk-dev/js2-mode-customizations)

(defun tk-dev/js2-mode-hook ()
  (setq mode-name "JS2"))

(add-hook 'js2-mode-hook #'tk-dev/js2-mode-hook)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.javascript\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))

(add-to-list 'interpreter-mode-alist '("node\\(?:js\\)?" . js2-mode))

;;; RJSX: js2-mode with jsx

(defun tk-dev/rjsx-mode-hook ()
  (setq mode-name "RJSX"))

(add-hook 'rjsx-mode-hook #'tk-dev/rjsx-mode-hook)

;;; ELisp

(defun tk-dev/emacs-lisp-mode-hook ()
  (setq mode-name "ELisp"))

(add-hook 'emacs-lisp-mode-hook #'tk-dev/emacs-lisp-mode-hook)

;;; Clojure

(defun tk-dev/clojure-mode-customizations ()
  (let ((default-indent 2))
    (define-clojure-indent
      (ANY       default-indent)
      (DELETE    default-indent)
      (GET       default-indent)
      (HEAD      default-indent)
      (OPTIONS   default-indent)
      (PATCH     default-indent)
      (POST      default-indent)
      (PUT       default-indent)
      (context   default-indent)
      (describe  default-indent)
      (it        default-indent)
      (defroutes 'defun))))

(eval-after-load 'clojure-mode #'tk-dev/clojure-mode-customizations)

;;; CIDER

(customize-set-variable 'cider-eval-result-prefix ";; => ")
(customize-set-variable 'cider-repl-result-prefix ";; => ")
(customize-set-variable 'cider-repl-history-file "~/.cider_history")

;; Attempt to use the symbol at point as input for `cider-find-var', and
;; only prompt if that throws an error
(customize-set-variable 'cider-prompt-for-symbol nil)

;; I want to inject dependencies manually via
;; `~/.lein/profiles.clj'. Otherwise Leiningen's `:pedantic? :abort'
;; setting causes `lein repl' to abort due to overriding version of
;; `org.clojure/tools.nrepl'.
(customize-set-variable 'cider-inject-dependencies-at-jack-in nil)

(custom-set-faces '(cider-result-overlay-face ((t (:background "grey30")))))

;; Shorten mode line info
(customize-set-variable 'cider-mode-line '(:eval (cider--modeline-info)))

;; Add related info to mode line
(defun tk-dev/cider-mode-customizations ()
  (add-to-list 'tk-looks/minor-mode-alist '(cider-popup-buffer-mode (" cider-tmp")))
  (add-to-list 'tk-looks/minor-mode-alist '(cider-auto-test-mode (cider-mode " Test")))
  (add-to-list 'tk-looks/minor-mode-alist '(cider-mode cider-mode-line))
  (add-to-list 'tk-looks/minor-mode-alist '(cider--debug-mode " DEBUG")))

(eval-after-load 'cider-mode #'tk-dev/cider-mode-customizations)

(defun tk-dev/cider-mode-hook ()
  (local-set-key (kbd "C-c B")   #'cider-connection-browser)
  (local-set-key (kbd "C-c M-R") #'cider-restart))

(add-hook 'cider-mode-hook #'tk-dev/cider-mode-hook)
(add-hook 'cider-repl-mode-hook #'tk-dev/cider-mode-hook)

;;; Paredit

(defun tk-dev/paredit-mode-customizations ()
  (define-key paredit-mode-map (kbd "C-<left>")  nil)
  (define-key paredit-mode-map (kbd "C-<right>") nil)
  (define-key paredit-mode-map (kbd "M-<left>")  #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-<right>") #'paredit-forward-slurp-sexp))

(eval-after-load 'paredit #'tk-dev/paredit-mode-customizations)

(add-hook 'clojure-mode-hook                     #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'ielm-mode-hook                        #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'paredit-mode)
(add-hook 'lisp-mode-hook                        #'paredit-mode)
(add-hook 'scheme-mode-hook                      #'paredit-mode)

(define-key prog-mode-map (kbd "C-c (") #'paredit-mode)

;;; CoffeeScript

(customize-set-variable 'coffee-tab-width 2)

;;; Haskell

(customize-set-variable 'haskell-process-suggest-remove-import-lines t)
(customize-set-variable 'haskell-process-auto-import-loaded-modules t)
(customize-set-variable 'haskell-process-log t)
(customize-set-variable 'haskell-process-type 'cabal-repl)

(defun tk-dev/haskell-mode-customizations ()
  (define-key haskell-mode-map (kbd "<f8>")  #'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-`")   #'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c c") #'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c o") #'haskell-hoogle))

(eval-after-load 'haskell-mode #'tk-dev/haskell-mode-customizations)

(defun tk-dev/haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (turn-on-haskell-decl-scan)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook #'tk-dev/haskell-mode-hook)

;;; Sass

(customize-set-variable 'scss-compile-at-save nil)

(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))

;;; Markdown

(customize-set-variable 'markdown-command "marked --gfm --tables")
(customize-set-variable 'markdown-hide-urls nil)
(customize-set-variable 'markdown-asymmetric-header t)
(customize-set-variable 'markdown-live-preview-delete-export 'delete-on-export)

(custom-set-faces '(markdown-code-face ((t (:background "#4b4b4b")))))

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;; Ruby

(add-to-list 'auto-mode-alist '("/gemfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/guardfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/procfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/rakefile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("/vagrantfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("j?ruby\\(?:1.8\\|1.9\\)?" . enh-ruby-mode))

;;; Shell script

(add-to-list 'auto-mode-alist '("\\.bashrc\\.[a-z0-9-.]+\\'" . sh-mode))

;;; SQL

(with-eval-after-load 'sql (load-library "sql-indent"))

;;; Textile

(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;; Visual Basic :D

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)

(add-to-list 'auto-mode-alist '("\\.aspx\\'" . visual-basic-mode))

;; Web

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))

;;; Interactive regexp builder

(global-set-key (kbd "C-c R") #'re-builder)

(customize-set-variable 'reb-re-syntax 'string)

(defun tk-dev/re-builder-mode-customizations ()
  (define-key reb-mode-map (kbd "M-n") #'reb-next-match)
  (define-key reb-mode-map (kbd "M-p") #'reb-prev-match))

(eval-after-load 're-builder #'tk-dev/re-builder-mode-customizations)

;;; Compilation

(define-key compilation-mode-map (kbd "M-N") #'compilation-next-file)
(define-key compilation-mode-map (kbd "M-P") #'compilation-previous-file)

;; Highlight color for next-error, used by `compilation-display-error'
(custom-set-faces '(next-error ((t (:background "SkyBlue3" :foreground "#dcdccc")))))

;;; Magit

(global-set-key (kbd "C-x g") #'magit-status)

(customize-set-variable 'magit-completing-read-function 'ivy-completing-read)

(custom-set-faces '(magit-diff-context ((t (:background "grey25"))))
                  '(magit-diff-context-highlight ((t (:background "grey32"))))
                  '(magit-diff-hunk-heading ((t (:background "#3e5f76"))))
                  '(magit-diff-hunk-heading-highlight ((t (:background "#619abf")))))

;; Disable `magit-auto-revert-mode', because we're using
;; global-auto-revert-mode
(customize-set-variable 'magit-auto-revert-mode nil)

;; Disable Emacs' Version Control interface
;; (customize-set-variable 'vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg Mtn))
(customize-set-variable 'vc-handled-backends nil)

;;; Flycheck

(customize-set-variable 'flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(global-flycheck-mode)

;;; Dash

(if (eq system-type 'darwin)
    (dolist (m (list text-mode-map prog-mode-map))
      (define-key m (kbd "C-c ?") #'dash-at-point)))