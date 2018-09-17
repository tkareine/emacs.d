;; -*- lexical-binding: t; -*-

(require 'cl-seq)
(require 'subr-x)

(defun tk-support/dotfile-path (&rest paths)
  "Expand file path components inside user emacs directory."
  (concat (expand-file-name user-emacs-directory)
          (mapconcat #'file-name-as-directory (butlast paths) "")
          (car (last paths))))

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

(defun tk-support/npm-global-path (&rest paths)
  "Expand file path components inside current npm global
installation directory."
  (concat (mapconcat #'file-name-as-directory
                     (append (list (string-trim (shell-command-to-string "nodenv prefix"))
                                   "lib/node_modules")
                             (butlast paths))
                     "")
          (car (last paths))))

(defun tk-support/active-region-or-line ()
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (line-beginning-position 2))))

(defun tk-support/pretty-print-xml (begin end)
  "Pretty format XML markup in region with nxml-mode."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (let* ((begin (or begin (point-min)))
         (end (or end (point-max)))
         (last-buf (current-buffer))
         (tmp-buf (generate-new-buffer (generate-new-buffer-name "*tkareine-pretty-print-xml-region*"))))
    (unwind-protect
        (save-excursion
          (with-current-buffer tmp-buf
            (insert-buffer-substring-no-properties last-buf begin end)
            (nxml-mode)
            (goto-char (point-min))
            ;; split `<foo><foo>' or `</foo><foo>', but not `<foo></foo>'
            (while (search-forward-regexp ">[ \t]*<[^/]" end t)
              (backward-char 2)
              (insert "\n"))
            ;; split `<foo/></foo>' and `</foo></foo>'
            (goto-char (point-min))
            (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
              (backward-char)
              (insert "\n"))
            (indent-region (point-min) (point-max) nil)
            (delete-trailing-whitespace (point-min) (point-max)))
          (delete-region begin end)
          (insert-buffer-substring tmp-buf))
    (kill-buffer tmp-buf))))

(defun tk-support/string-prefix-length-with-char (char str)
  (let ((str-len (length str))
        (idx 0)
        (should-continue t))
    (while (and should-continue (< idx str-len))
      (if (char-equal (aref str idx) char)
          (setq idx (1+ idx))
        (setq should-continue nil)))
    idx))

(provide 'tk-support)
