;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; switch.scm - Scheme environment switch command script
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

(import (rnrs)
	(util file)
	(sagittarius))

(define (print . args) (for-each display args) (newline))
(define scheme-env-home
  (or (getenv "SCHEME_ENV_HOME")
      (begin (print "invalid call") (exit -1))))
(define bin-directory (build-path scheme-env-home "bin"))

(define (replace-symlink name)
  (define default (build-path bin-directory "default"))
  (define target (build-path bin-directory name))

  (cond ((file-exists? target)
	 (when (file-exists? default) (delete-file default))
	 (create-symbolic-link target default)
	 (print "Switched to " name))
	(else
	 (print "Implementation " name " doesn't exist"))))

(define (usage)
  (print "scheme-env switch implementation")
  (print " Swiching default implementation"))

(define (main args)
  (when (or (null? args) (not (null? (cdr args)))) (usage))
  (case (string->symbol (car args))
    ((default host-scheme scheme-env)
     (print "System value cannot be specified")
     (exit -1))
    (else (replace-symlink (car args)))))
