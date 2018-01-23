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
	(rnrs eval)
	(sagittarius)
	(sagittarius regex)
	(sagittarius process)
	(scheme load)
	(archive)
	(rfc http)
	(rfc gzip)
	(util file)
	(srfi :26)
	(srfi :39))

(define (print . args) (for-each display args) (newline))
(define (parse-version arg)
  (cond ((#/([^@]+)@(.+)/ arg) => (lambda (m) (values (m 1) (m 2))))
	(else (values arg #f))))

(define scheme-env-home
  (or (getenv "SCHEME_ENV_HOME")
      (begin (print "invalid call") (exit -1))))

(define-constant +default-github-repository+
  "https://raw.githubusercontent.com/ktakashi/scheme-env/master/scripts/install")

(define (invoke-installer implementation version)
  (define repository
    (cond ((getenv "SCHEME_ENV_REPOSITORY") =>
	   (lambda (r) (build-path* r "scripts" "install")))
	  (else +default-github-repository+)))
  (define (->implementation-file base) (format "~a/~a.scm" base implementation))
  
  (define (get-file implementation-file repository)
    (define (download m)
      (let-values (((s h b)
		    (http-get (m 2) (->implementation-file (m 3))
			      :secure (string=? (m 1) "https"))))
	(unless (string=? s "200")
	  (assertion-violation 'scheme-env "implementation not found"
			       implementation))
	(call-with-output-file implementation-file
	  (lambda (out) (put-string out b)))
	implementation-file))
    (cond ((#/(https?):\/\/([^\/]+)(.+)/ repository) =>
	   (lambda (m)
	     (cond ((file-exists? implementation-file) implementation-file)
		   (else (download m)))))
	  (else
	   (let ((local (->implementation-file repository)))
	     (cond ((file-exists? local)
		    (copy-file local implementation-file #t)
		    implementation-file)
		   ((file-exists? implementation-file) implementation-file)
		   (else
		    (assertion-violation 'scheme-env
					 "implementation not found in local"
					 implementation)))))))
  (define local-repository (build-path* scheme-env-home "scripts" "install"))
  
  (unless (file-exists? local-repository) (create-directory* local-repository))
  (let ((file (get-file (->implementation-file local-repository) repository))
	(env (environment '(only (sagittarius) import library define-library))))
    (load file env)
    (eval `(install ,version) env)))

(define (usage)
  (print "scheme-env install implementation ...")
  (print " implementation format")
  (print "  - implementation")
  (print "  - implementation@version")
  (print " Supporting implementations")
  (print "  - chibi-scheme")
  (print "  - sagittarius")
  (exit -1))

(define (main args)
  (when (null? args) (usage))
  (for-each (lambda (implementation)
	      (let-values (((impl version) (parse-version implementation)))
		(invoke-installer impl version)))
	    args))
