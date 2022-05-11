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
	(scheme load)
	(tools))

(define (print . args) (for-each display args) (newline))

(define (invoke-installer implementation version)
  (let ((file (scheme-env:script-file (format "install/~a" implementation)))
	(env (environment '(only (sagittarius) import library define-library))))
    (load file env)
    (eval `(install ,version) env)))

(define (usage)
  (define p scheme-env:print)
  (p "scheme-env install <implementation> ...")
  (p)
  (p "Description")
  (p " Installs specified <implementation>s.")
  (p)
  (p " <implementation> format")
  (p "  - implementation")
  (p "  - implementation@version")
  (p)
  (p " Supporting implementations")
  (p "  - chez")
  (p "  - chibi-scheme")
  (p "  - foment")
  (p "  - gauche")
  (p "  - larceny")
  (p "  - sagittarius")
  (exit -1))

(define (main args)
  (when (null? args) (usage))
  (for-each (lambda (implementation)
	      (let-values (((impl version)
			    (scheme-env:parse-version implementation)))
		(invoke-installer impl version)))
	    args))
