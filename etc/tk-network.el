;; -*- lexical-binding: t; -*-

(defun tk-network/test-tls-security ()
  "Adapted from URL
`https://glyph.twistedmatrix.com/2015/11/editor-malware.html'."
  (interactive)
  (let* ((bad-hosts (cl-loop for bad-url
                             in '("https://wrong.host.badssl.com/"
                                  "https://self-signed.badssl.com/")
                             if (ignore-errors (url-retrieve-synchronously bad-url nil t 10))
                             collect bad-url)))
    (if bad-hosts
        (error (format "TLS security test failed: should've failed retrieving %s"
                       bad-hosts))
      (url-retrieve "https://melpa.org"
                    (lambda (_retrieved) t)))
    (message "TLS security test passed")))

;; Network Security Manager
(customize-set-variable 'network-security-level 'medium)

;; tls via external tool: check certificates
(customize-set-variable 'tls-checktrust t)

;; tls via external tool: use openssl/libressl
(customize-set-variable 'tls-program '("openssl s_client -connect %h:%p -CAfile %t -no_ssl2 -no_ssl3 -ign_eof -verify 9"))

;; tls via builtin gnutls: check certificates
(customize-set-variable 'gnutls-verify-error t)

;; tls via builtin gnutls and external tool: the cert files used
(customize-set-variable 'gnutls-trustfiles (list (expand-file-name "~/brew/etc/libressl/cert.pem")))
