;; -*- lexical-binding: t; -*-

;; Number of bytes of consing between garbage collections. Increased
;; from the original value to avoid frequent GC pauses with
;; lsp-mode. From
;; `https://emacs-lsp.github.io/lsp-mode/page/performance/'.
(customize-set-variable 'gc-cons-threshold 10000000)
