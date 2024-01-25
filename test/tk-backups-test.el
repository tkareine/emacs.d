;; -*- lexical-binding: t -*-

(require 'tk-init)

(load-file (tk-init/user-emacs-path "etc" "tk-backups.el"))

(ert-deftest tk-backups/backup-file-enable-p-test ()
  (should (tk-backups/backup-file-enable-p "/var/foo.txt"))
  (should-not (tk-backups/backup-file-enable-p "/tmp/foo.txt"))
  (should-not (tk-backups/backup-file-enable-p "/docker:container:/foo.txt"))
  (should-not (tk-backups/backup-file-enable-p "/sudo:root@host:/foo.txt")))
