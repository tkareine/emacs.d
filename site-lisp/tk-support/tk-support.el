;; -*- lexical-binding: t; -*-

(defun tk-support/active-region-or-line ()
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (line-beginning-position 2))))

(declare-function cl-find-if "cl-seq")

(defun tk-support/locate-any-dominating-file (file names)
  "Starting at FILE, look up file path for directory containing
any of NAMES. Stop at the first parent directory containing the
file, and return the expanded file name of the file path. Return
nil if not found."
  (let ((found))
    (locate-dominating-file file
                            (lambda (dir)
                              (cl-find-if (lambda (name)
                                            (let* ((path   (expand-file-name name dir))
                                                   (exists (file-exists-p path)))
                                              (when exists
                                                (setq found path))
                                              exists))
                                          names)))
    found))

(defun tk-support/string-prefix-length-with-char (char str)
  (let ((str-len (length str))
        (idx 0)
        (should-continue t))
    (while (and should-continue (< idx str-len))
      (if (char-equal (aref str idx) char)
          (setq idx (1+ idx))
        (setq should-continue nil)))
    idx))

;; For `cl-flet' in `tk-support/xml-pretty-print'
(eval-when-compile
  (require 'cl-macs))

(defun tk-support/xml-pretty-print (begin end)
  "Pretty format XML markup in region with nxml-mode."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (let* ((begin (or begin (point-min)))
         (end (or end (point-max)))
         (last-buf (current-buffer))
         (tmp-buf (generate-new-buffer (generate-new-buffer-name "*tk-support-xml-pretty-print-region*"))))
    (cl-flet ((split-at-regex-all (regexp relative-split-point)
                (goto-char (point-min))
                (while (search-forward-regexp regexp nil t)
                  (backward-char relative-split-point)
                  (insert "\n"))))
      (unwind-protect
          (save-excursion
            (with-current-buffer tmp-buf
              (insert-buffer-substring-no-properties last-buf begin end)
              (nxml-mode)
              ;; split `<foo><foo>' or `</foo><foo>', but not `<foo></foo>'
              (split-at-regex-all ">[ \t]*<[^/]" 2)
              ;; split `<foo/></foo>' or `</foo></foo>'
              (split-at-regex-all "<[^/>]*/[^/>]*>[ \t]*</" 2)
              (indent-region (point-min) (point-max) nil)
              (delete-trailing-whitespace (point-min) (point-max)))
            (delete-region begin end)
            (insert-buffer-substring tmp-buf))
        (kill-buffer tmp-buf)))))

(provide 'tk-support)
