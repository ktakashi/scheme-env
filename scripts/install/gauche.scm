#!nounbound
(import (rnrs)
	(sagittarius)
	(sagittarius process)
	(rfc http)
	(util file)
	(tools)
	(srfi :39))

(define (print . args) (for-each display args) (newline))

;; designator can be #f, "latest" or "snapshot"
(define (get-real-version designator)
  (let-values (((s h b)
                (http-get "practical-scheme.net"
                          (format "/gauche/releases/~a.txt"
                                  (or designator "latest"))
                          :secure #t)))
    (unless (string=? s "200")
      (assertion-violation 'gauche "Failed to get latest version of Gauche" s h))
    b))

(define (get-get-gauche filename)
  (let-values (((s h b)
                (http-get "raw.githubusercontent.com"
                          "/shirok/get-gauche/master/get-gauche.sh"
                          :receiver (http-file-receiver filename :temporary? #t)
                          :secure #t)))
    (unless (string=? s "200")
      (assertion-violation 'gauche "Failed to get get-gauche" s h))
    (change-file-mode b #o775)
    b))

(define (install version)
  (define real-version
    (if (member version '(#f "latest" "snapshot"))
      (get-real-version version)
      version))
  (define install-prefix
    (build-path* (scheme-env-implentations-directory) "gauche" real-version))
  (scheme-env:with-work-directory "gauche"
    (lambda (work-dir)
      (let ((get-gauche.sh (get-get-gauche
			    (build-path* work-dir "get-gauche.sh"))))
	(run get-gauche.sh
	     "--prefix" install-prefix
	     "--version" real-version
	     "--force" "--auto"))))
  (let ((new (build-path* (scheme-env-home) "bin"
                          (format "gauche~a"
                                  (string-append "@" real-version))))
        (bin (build-path* install-prefix "bin" "gosh")))
    (when (file-exists? new) (delete-file new))
    (create-symbolic-link bin new))
  (print "Gauche is installed"))


