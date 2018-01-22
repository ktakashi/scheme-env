;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; install.scm - Scheme environment install command script
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!read-macro=sagittarius/regex
(import (rnrs)
	(sagittarius)
	(sagittarius regex)
	(sagittarius process)
	(archive)
	(rfc http)
	(rfc gzip)
	(util file)
	(srfi :26)
	(srfi :39))

(define (print . args) (for-each display args) (newline))
(define scheme-env-home
  (or (getenv "SCHEME_ENV_HOME")
      (begin (print "invalid call") (exit -1))))
(define work-directory (build-path scheme-env-home "work"))

(define (call-with-safe-output-file file proc)
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file proc))

;;(define (destinator e)
;;  (let ((name (archive-entry-name e)))
;;    (format #t "-- Extracting: ~a~%" name)
;;    name))

;; Got an error of bar tar header checksum...
;; (define (install-chibi-scheme version)
;;   (define real-version (or version "master"))
;;   (let-values (((s h b)
;; 		(http-get "github.com"
;; 			  (format "/ashinn/chibi-scheme/archive/~a.tar.gz"
;; 				  real-version)
;; 			  :receiver (http-binary-receiver)
;; 			  :secure #t)))
;;     (unless (string=? s "200")
;;       (assertion-violation 'chibi-scheme "Failed to download Chibi Scheme" s h))
;;     (let ((work-dir (build-path* work-directory "chibi-scheme" real-version))
;; 	  (in (open-gzip-input-port (open-bytevector-input-port b))))
;;       (create-directory* work-dir)
;;       (parameterize ((current-directory work-dir))
;; 	(call-with-archive-input 'tar in
;; 	  (cut extract-all-entries <> :overwrite #t :destinator destinator))
;; 	(process-wait (process-call "make"))
;; 	(process-wait (process-call "make install"))))))

;; so use process
(define (install-chibi-scheme version)
  (define real-version (or version "master"))
  (define install-prefix
    (build-path* scheme-env-home "implementations" "chibi-scheme" real-version))
  (define (download dir)
    (define file (build-path dir "chibi-scheme"))
    (let-values (((s h file)
		  (http-get "github.com"
			    (format "/ashinn/chibi-scheme/archive/~a.tar.gz"
				    real-version)
			    :receiver (http-file-receiver file)
			    :secure #t)))
      (unless (string=? s "200")
	(assertion-violation 'chibi-scheme
			     "Failed to download Chibi Scheme" s h))
      (print file)
      (run "tar" "xvf" file)
      (delete-file file)))
  (let ((work-dir (build-path* work-directory "chibi-scheme" real-version))
	(prefix (format "PREFIX=~a" install-prefix)))
    (delete-directory* work-dir)
    (create-directory* work-dir)
    (parameterize ((current-directory work-dir))
      (download work-dir)
      (path-for-each "." (lambda (path type)
			   (case type
			     ((directory)
			      (parameterize ((current-directory path))
				(run "make" prefix)
				(run "make" prefix "install")))))
		     :recursive #f))
    (let ((new (build-path* scheme-env-home "bin" "chibi-scheme"))
	  (old (build-path* install-prefix "chibi-scheme"))
	  (bin (build-path* install-prefix "bin" "chibi-scheme"))
	  (lib (build-path* install-prefix "lib")))
      (call-with-safe-output-file old
        (lambda (out)
	  (put-string out "#!/bin/sh\n")
	  (format out "LD_LIBRARY_PATH=~a ~a \"$@\"" lib bin)))
      (change-file-mode old #o775)
      (when (file-exists? new) (delete-file new))
      (create-symbolic-link old new)
      )))

(define (parse-version arg)
  (cond ((#/([^@]+)@(.+)/ arg) => (lambda (m) (values (m 1) (m 2))))
	(else (values arg #f))))

(define (usage)
  (print "scheme-env install implementation ...")
  (print " implementation format")
  (print "  - implementation")
  (print "  - implementation@version")
  (print " Supporting implementations")
  (print "  - chibi-scheme")
;; (print "  - sagittarius")
  (exit -1))

(define (main args)
  (when (null? args) (usage))
  (for-each (lambda (implementation)
	      (let-values (((impl version) (parse-version implementation)))
		(case (string->symbol impl)
		  ((chibi-scheme) (install-chibi-scheme version))
		  (else (print "Not supported: " impl)))))
	    args))
