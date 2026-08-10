((all-the-icons :source "elpaca-menu-lock-file" :recipe
                (:package "all-the-icons" :repo
                          "domtronn/all-the-icons.el" :fetcher github
                          :files (:defaults "data") :source
                          "elpaca-menu-lock-file" :id all-the-icons
                          :type git :protocol https :inherit t :depth
                          treeless :ref
                          "4778632b29c8c8d2b7cd9ce69535d0be01d846f9"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo
                     "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters"))
                     :source "elpaca-menu-lock-file" :id apheleia :type
                     git :protocol https :inherit t :depth treeless :ref
                     "0d64f2c99d886bf7aa1a64a2aa73d52bb8e8f912"))
 (base16-theme :source "elpaca-menu-lock-file" :recipe
               (:package "base16-theme" :repo
                         "tinted-theming/base16-emacs" :fetcher github
                         :files (:defaults "build/*.el") :source
                         "elpaca-menu-lock-file" :id base16-theme :type
                         git :protocol https :inherit t :depth treeless
                         :ref "53a8b553b364964ffb97c5f84de34f01e4d5b2eb"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo
                   ("https://github.com/emacs-compat/compat" . "compat")
                   :tar "31.0.0.2" :host gnu :files
                   ("*" (:exclude ".git")) :source
                   "elpaca-menu-lock-file" :id compat :type git
                   :protocol https :inherit t :depth treeless :ref
                   "1f3d7596173cf2851d3c4181b15d6c3573a38252"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "elpaca-menu-lock-file" :id cond-let :type git
             :protocol https :inherit t :depth treeless :ref
             "c48600dfab6372670225f046cace263700c78eab"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id consult :type
                    git :protocol https :inherit t :depth treeless :ref
                    "3ddec5493bce5445f099537be50b7a4f79c68321"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  (:defaults "extensions/corfu-*.el") :fetcher github
                  :source "elpaca-menu-lock-file" :id corfu :type git
                  :protocol https :inherit t :depth treeless :ref
                  "4b440fb30ff2fff4291af676b59da7ab22130a45"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "elpaca-menu-lock-file"
                 :id dash :type git :protocol https :inherit t :depth
                 treeless :ref
                 "d746dd9edcb67a108818beb0cdc78dc1cb466832"))
 (deadgrep :source "elpaca-menu-lock-file" :recipe
           (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id deadgrep :type
                     git :protocol https :inherit t :depth treeless :ref
                     "85257234fa696f412c5701b94f3aa5266a760252"))
 (dirvish :source "elpaca-menu-lock-file" :recipe
          (:package "dirvish" :fetcher github :repo "alexluigit/dirvish"
                    :files (:defaults "extensions/*.el") :source
                    "elpaca-menu-lock-file" :id dirvish :type git
                    :protocol https :inherit t :depth treeless :ref
                    "bf164ee21e128837ede59be03836a9900c4a41be"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "6530ffa73b18ccee858e7c471415ab7e0c0d8ce1" :depth 1 :inherit
            ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main "extensions/elpaca-use-package.el"
                               :build
                               (:not elpaca-source elpaca-build-docs)
                               :source "elpaca-menu-lock-file" :id
                               elpaca-use-package :type git :protocol
                               https :inherit t :depth treeless :ref
                               "6530ffa73b18ccee858e7c471415ab7e0c0d8ce1"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo
                          "magnars/expand-region.el" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id
                          expand-region :type git :protocol https
                          :inherit t :depth treeless :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id f :type git :protocol
              https :inherit t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id flycheck :type
                     git :protocol https :inherit t :depth treeless :ref
                     "e6fe520a21df70854055468a77c2dfe8adf02297"))
 (ggtags :source "elpaca-menu-lock-file" :recipe
         (:package "ggtags" :fetcher github :repo "leoliu/ggtags" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id ggtags :type git
                   :protocol https :inherit t :depth treeless :ref
                   "4e3630c30fb836872b5d8f2ae3e5d5ae003365d8"))
 (git-gutter :source "elpaca-menu-lock-file" :recipe
             (:package "git-gutter" :repo "emacsorphanage/git-gutter"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id git-gutter
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "101b1e29ec4f4609b29a17877990f95993452188"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :id ht :type git
               :protocol https :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source
                  "elpaca-menu-lock-file" :id llama :type git :protocol
                  https :inherit t :depth treeless :ref
                  "4d4024048053b898a01521046e0f063ee47615b0"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
                     github :files (:defaults "clients/*.*") :source
                     "elpaca-menu-lock-file" :id lsp-mode :type git
                     :protocol https :inherit t :depth treeless :ref
                     "e15b8205cbd0369df40b412909eb3ed3264e96a2"))
 (lsp-ui :source "elpaca-menu-lock-file" :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github
                   :files (:defaults "lsp-ui-doc.html" "resources")
                   :source "elpaca-menu-lock-file" :id lsp-ui :type git
                   :protocol https :inherit t :depth treeless :ref
                   "176eca71d1c5498ed6258b5b27d73293ff7cd7ed"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "elpaca-menu-lock-file" :id lv :type
               git :protocol https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (macrostep :source "elpaca-menu-lock-file" :recipe
            (:package "macrostep" :fetcher github :repo
                      "emacsorphanage/macrostep" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id macrostep
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "d0928626b4711dcf9f8f90439d23701118724199"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("githooks" "githooks/*") ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :id magit :type git
                  :protocol https :inherit t :depth treeless :ref
                  "b1742562074e794348bdc56da838ac9d3ae40364"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :id
                          magit-section :type git :protocol https
                          :inherit t :depth treeless :ref
                          "b1742562074e794348bdc56da838ac9d3ae40364"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id marginalia
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "10b170ad8006bad535599e5b3e007e643e34345a"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id
                          markdown-mode :type git :protocol https
                          :inherit t :depth treeless :ref
                          "f441e8bc9951e73b12c61e9198658488dd8e86e1"))
 (olivetti :source "elpaca-menu-lock-file" :recipe
           (:package "olivetti" :fetcher github :repo "rnkn/olivetti"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id olivetti :type
                     git :protocol https :inherit t :depth treeless :ref
                     "d2ccae56b442d9c5b06dd2481057abbd7eb82551"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id orderless
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "0ffd9d6903714c1f6d8fcbb6a20941fb33dd7ae5"))
 (projectile :source "elpaca-menu-lock-file" :recipe
             (:package "projectile" :fetcher github :repo
                       "bbatsov/projectile" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id projectile
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "f954c2f0f347500e1b5b482d9492075447de5256"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github
                               :repo "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info"
                                "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                "docs/dir" "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id
                               rainbow-delimiters :type git :protocol
                               https :inherit t :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id rust-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "0058837c048cc031ca1a13f598a6a6604777458b"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id s :type git :protocol
              https :inherit t :depth treeless :ref
              "d7c04b84d03481a1ed62ee13dbe595224ccbe57c"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo
                        "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id smartparens
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :tar "1.7.4" :host gnu :files
                    ("*" (:exclude ".git")) :source
                    "elpaca-menu-lock-file" :id spinner :type git
                    :protocol https :inherit t :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (symbol-overlay :source "elpaca-menu-lock-file" :recipe
                 (:package "symbol-overlay" :fetcher github :repo
                           "wolray/symbol-overlay" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :id
                           symbol-overlay :type git :protocol https
                           :inherit t :depth treeless :ref
                           "85d100b0cca35b70cee1b260e09af8e1fb2fcc08"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id transient
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "0ec75dcce235f5ab3d39a02b878e6aaa78159b22"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "elpaca-menu-lock-file" :id vertico
                    :type git :protocol https :inherit t :depth treeless
                    :ref "be96000c2b0b3501723291b3721ceba12f784dcd"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo
                  ("https://github.com/casouri/vundo" . "vundo") :tar
                  "2.4.0" :host gnu :files
                  ("*" (:exclude ".git" "test")) :source
                  "elpaca-menu-lock-file" :id vundo :type git :protocol
                  https :inherit t :depth treeless :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (which-key :source "elpaca-menu-lock-file" :recipe
            (:package "which-key" :repo "justbur/emacs-which-key"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id which-key
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "38d4308d1143b61e4004b6e7a940686784e51500"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo
                        "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id with-editor
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "a1f92a26e53033ec58e1d2ce9b132da7ebae816e"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :fetcher github :repo
                      "joaotavora/yasnippet" :files
                      (:defaults ("doc" "doc/*.org")) :source
                      "elpaca-menu-lock-file" :id yasnippet :type git
                      :protocol https :inherit t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37")))
