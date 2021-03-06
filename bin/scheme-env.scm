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

;; a bit of duplication
(define-constant +default-github-repository+
  "https://raw.githubusercontent.com/ktakashi/scheme-env/master")
(define scheme-env-repository
  (cond ((getenv "SCHEME_ENV_REPOSITORY"))
	(else +default-github-repository+)))
(define scheme-env-home
  (or (getenv "SCHEME_ENV_HOME")
      (assertion-violation 'scheme-env-home "SCHEME_ENV_HOME is not set")))

(define (load-tools-library)
  (define destination-directory (build-path scheme-env-home "lib"))
  (define output-file (build-path destination-directory "tools.scm"))
  (define repository scheme-env-repository)
  (define (download m)
    (let-values (((s h b)
		  (http-get (m 2) (string-append (m 3) "/lib/tools.scm")
			    :secure (string=? (m 1) "https"))))
      (unless (string=? s "200")
	(assertion-violation 'scheme-env "tools library not found"))
      (call-with-output-file output-file
	(lambda (out) (put-string out b)))
      output-file))
  (define (retrieve-file)
    (cond ((#/(https?):\/\/([^\/]+)(.+)/ repository) =>
	   (lambda (m)
	     (cond ((file-exists? output-file) output-file)
		   (else (download m)))))
	  (else
	   (let ((repository-file (build-path* repository "lib" "tools.scm")))
	     (cond ((file-exists? repository-file)
		    (copy-file repository-file output-file #t)
		    output-file)
		   ((file-exists? repository-file) repository-file)
		   (else
		    (assertion-violation 'load-tools-library
		      "Tools library file not found in specified repository" 
		      )))))))
  (unless (file-exists? destination-directory)
    (create-directory* destination-directory))
  (let ((tools (retrieve-file)))
    (load tools)))

(define (invoke-command command args)
  (load-tools-library)
  ;; ok we need to specify the library
  (let ((file (eval `(scheme-env:script-file ',command)
		    (environment '(rnrs) '(tools))))
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
