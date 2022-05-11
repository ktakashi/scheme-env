;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; list.scm - Scheme environment list command script
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
	(tools)
	(getopt)
	(srfi :13))

(define (usage)
  (define p scheme-env:print)
  (p "scheme-env list [-l]")
  (p)
  (p "Description")
  (p " Lists all the installed implementations")
  (p)
  (p " -l,--list")
  (p "  Do not put any excess messaage.")
  (exit -1))

(define (main args)
  (with-args args
      ((list? (#\l "list") #f #f)
       . ignore)
    (unless list? (scheme-env:print "Installed implementations:"))
    (path-for-each (scheme-env-bin-directory)
		   (lambda (path type)
		     (when (string-contains path "@")
		       (case (string->symbol path)
			 ((default host-scheme scheme-env))
			 (else
			  (if list?
			      (scheme-env:print path)
			      (scheme-env:print "    " path))))))
		   :recursive #f
		   :absolute-path #f)))
		      
