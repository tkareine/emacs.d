(defun tkareine/add-dotfile-to-load-path (p)
  (add-to-list 'load-path (tkareine/dotfile-path p)))

(defun tkareine/filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun tkareine/active-region-or-line ()
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (line-beginning-position 2))))

(defun tkareine/pretty-print-xml (begin end)
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
            ;; split <foo><foo> or </foo><foo>, but not <foo></foo>
            (while (search-forward-regexp ">[ \t]*<[^/]" end t)
              (backward-char 2)
              (insert "\n"))
            ;; split <foo/></foo> and </foo></foo>
            (goto-char (point-min))
            (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
              (backward-char)
              (insert "\n"))
            (indent-region (point-min) (point-max) nil)
            (delete-trailing-whitespace (point-min) (point-max)))
          (delete-region begin end)
          (insert-buffer-substring tmp-buf))
    (kill-buffer tmp-buf))))

