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
	(rfc http)
	(util file))

(define-constant +default-github-repository+
  "https://raw.githubusercontent.com/ktakashi/scheme-env/master/scripts")

(define (invoke-command command args)
  (define repository
    (cond ((getenv "SCHEME_ENV_REPOSITORY") =>
	   (lambda (r) (build-path r "scripts")))
	  (else +default-github-repository+)))
  (define home
    (or (getenv "SCHEME_ENV_HOME")
	(assertion-violation 'scheme-env "SCHEME_ENV_HOME is not set")))
  (define (->command-file base) (format "~a/~a.scm" base command))

  (define (get-file command-file repository)
    (define (download m)
      (let-values (((s h b)
		    (http-get (m 2) (->command-file (m 3))
			      :secure (string=? (m 1) "https"))))
	(unless (string=? s "200")
	  (assertion-violation 'scheme-env "command not found" command))
	(call-with-output-file command-file
	  (lambda (out) (put-string out b)))
	command-file))
    (cond ((#/(https?):\/\/([^\/]+)(.+)/ repository) =>
	   (lambda (m)
	     (cond ((file-exists? command-file) command-file)
		   (else (download m)))))
	  (else
	   (let ((local (->command-file repository)))
	     (cond ((file-exists? local)
		    (copy-file local command-file #t)
		    command-file)
		   ((file-exists? command-file) command-file)
		   (else
		    (assertion-violation 'scheme-env
					 "command not found in local"
					 command)))))))
  (define local-repository (build-path home "scripts"))

  (unless (file-exists? local-repository) (create-directory* local-repository))
  (let ((file (get-file (->command-file local-repository) repository))
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
