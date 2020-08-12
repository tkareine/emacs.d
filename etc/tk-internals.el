;; -*- lexical-binding: t; -*-

;; Number of bytes of consing between garbage collections. Increased
;; from the original value to avoid frequent GC pauses with
;; lsp-mode. From
;; `https://emacs-lsp.github.io/lsp-mode/page/performance/'.
(customize-set-variable 'gc-cons-threshold 10000000)

;; Maximum number of bytes to read from subprocess in a single chunk.
(setq read-process-output-max (* 1024 1024))
