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
(customize-set-variable 'network-security-level 'medium)

;; tls via external tool: check certificates
(customize-set-variable 'tls-checktrust t)

;; tls via external tool: use openssl/libressl
(customize-set-variable 'tls-program '("openssl s_client -connect %h:%p -CAfile %t -no_ssl2 -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof -verify 9"))

;; tls via builtin gnutls: check certificates
(customize-set-variable 'gnutls-verify-error t)

;; tls via builtin gnutls and external tool: the cert files used
(customize-set-variable 'gnutls-trustfiles (list (expand-file-name "~/brew/etc/libressl/cert.pem")))
