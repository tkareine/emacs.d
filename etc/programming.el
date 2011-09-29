;; JavaScript language customizations
(setq js-indent-level 2)

;; Erlang language support
(let* ((root-dir  (concat my-dotfiles-lib-dir "erlang-mode"))
       (bin-dir   (concat root-dir "bin"))
       (elisp-dir (car (file-expand-wildcards (concat root-dir
                                                      "/lib/tools-2.6.*/emacs") t))))
  (setq erlang-root-dir root-dir)
  (add-to-list 'load-path elisp-dir)
  (add-to-list 'exec-path bin-dir))
(require 'erlang-start)

;; Scala language support
(my-add-dotfile-lib-path "scala-mode")
(require 'scala-mode-auto)
