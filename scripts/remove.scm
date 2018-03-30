;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; remove.scm - Scheme environment remove command script
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
	(sagittarius)
	(util file)
	(srfi :13)
	(tools))

(define (usage)
  (scheme-env:print "scheme-env remove implementation ...")
  (scheme-env:print " To check the installed implementation, please use `list` command")
  (exit -1))

(define (remove-implementation impl)
  (define bin (build-path (scheme-env-bin-directory) impl))
  (define (err msg . irr)
    (scheme-env:print msg)
    (for-each (lambda (i) (scheme-env:print "  irritant: " i)) irr))
  (define (do-remove symlink)
    (let-values (((name version) (scheme-env:parse-version impl)))
      (delete-directory* (build-path* (scheme-env-implentations-directory)
				      name version))
      (delete-file symlink)
      (scheme-env:print impl " has been removed")))
  (if (and (string-contains impl "@") (file-exists? bin))
      (let ((host-scheme (absolute-path (scheme-env-host-implementation)))
	    (target (absolute-path bin)))
	(if (equal? host-scheme target)
	    (err "Attempt to remove host scheme" impl)
	    (do-remove bin)))
      (err "Invalid file" bin)))

(define (main args)
  (when (null? args) (usage))
  (for-each remove-implementation args))
