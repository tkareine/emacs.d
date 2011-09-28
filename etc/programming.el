;; Erlang language support
(let* ((root-dir  (concat dotfiles-lib-dir "erlang-mode"))
       (bin-dir   (concat root-dir "bin"))
       (elisp-dir (car (file-expand-wildcards (concat root-dir
                                                      "/lib/tools-2.6.*/emacs") t))))
  (setq erlang-root-dir root-dir)
  (add-to-list 'load-path elisp-dir)
  (add-to-list 'exec-path bin-dir))
(require 'erlang-start)

;; Scala language support
(add-dotfile-lib-path "scala-mode")
(require 'scala-mode-auto)
