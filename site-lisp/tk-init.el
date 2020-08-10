;; -*- lexical-binding: t; -*-

(eval-and-compile
  (defun tk-init/user-emacs-path (&rest paths)
    "Expand file path components inside user emacs directory."
    (let ((sub-path (concat (mapconcat #'file-name-as-directory (butlast paths) "")
                            (car (last paths)))))
      (expand-file-name sub-path user-emacs-directory))))

(provide 'tk-init)
