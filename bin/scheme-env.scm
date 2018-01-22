;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; scheme-env.scm - Scheme environment command invocation script
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
	(scheme load)
	(rfc http))

(define-constant +default-github-repository+
  "https://raw.githubusercontent.com/ktakashi/scheme-env/master/scripts")

(define (invoke-command command args)
  (define repository
    (or (getenv "SCHEME_ENV_REPOSITORY") +default-github-repository+))
  (define home
    (or (getenv "SCHEME_ENV_HOME")
	(assertion-violation 'scheme-env "SCHEME_ENV_HOME is not set")))
  (define (->command-file base) (format "~a/~a.scm" base command))
  
  (define (get-file command-file repository)
    (cond ((file-exists? command-file) command-file)
	  ((#/(https?):\/\/([^\/]+)(.+)/ repository) =>
	   (lambda (m)
	     (let-values (((s h b)
			   (http-get (m 2) (->command-file (m 3))
				     :secure? (string=? (m 1) "https"))))
	       (unless (string=? s "200")
		 (assertion-violation 'scheme-env "command not found" command))
	       (call-with-output-file command-file
		 (lambda (out) (put-string out b)))
	       command-file)))
	  (else
	   (let ((local (->command-file repository)))
	     (display local) (newline)
	     (unless (file-exists? local)
	       (assertion-violation 'scheme-env
				    "command not found in local" command))
	     (copy-file local command-file #f)
	     command-file))))
  
  (let ((file (get-file (->command-file (build-path home "scripts"))
			repository))
	(env (environment '(only (sagittarius) import library define-library))))
    (load file env)
    (eval `(main ',args) env)))

(define (usage)
  (print "scheme-env command [OPTIONS]")
  (exit -1))

(define (main args)
  (when (null? (cdr args)) (usage))
  (case (string->symbol (cadr args))
    ((help) (usage))
    (else => (lambda (command) (invoke-command command (cddr args))))))
