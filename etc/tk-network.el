;; -*- lexical-binding: t; -*-

(defun tk-network/test-tls-security ()
  "Adapted from URL
`https://glyph.twistedmatrix.com/2015/11/editor-malware.html'."
  (interactive)
  (let* ((bad-urls (cl-loop for (check-url should-be-ok)
                            in '(("https://badssl.com/" t)
                                 ("https://wrong.host.badssl.com/" nil)
                                 ("https://self-signed.badssl.com/" nil))
                            if (let* ((buf (ignore-errors (url-retrieve-synchronously check-url t t 10))))
                                 (if should-be-ok
                                     (not buf)
                                   buf))
                            collect `(,check-url ,should-be-ok))))
    (if bad-urls
        (error (format "TLS security test failed for URLs: %s" bad-urls))
      (message "TLS security test passed"))))

;; Network Security Manager
(setq-default network-security-level 'medium)

;; tls via external tool: check certificates
(setq-default tls-checktrust t)

;; tls via builtin gnutls: check certificates
(setq-default gnutls-verify-error t)
