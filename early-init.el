;; -*- lexical-binding: t; -*-

;; The plist mode of compilation for lsp-mode should perform better than
;; the default hash-table mode. From
;; `https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization'.
(setenv "LSP_USE_PLISTS" "true")

;; Number of bytes of consing between garbage collections. Increased
;; from the original value to avoid frequent GC pauses with lsp-mode.
;; From
;; `https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold'.
(setq-default gc-cons-threshold
              (let ((mb (* 1024 1024))) (* 10 mb)))

;; Maximum number of bytes to read from subprocess in a single chunk.
;; From
;; `https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process'.
(setq-default read-process-output-max
              (let ((mb (* 1024 1024))) (* 1 mb)))
