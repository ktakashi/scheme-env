;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; install/foment.scm - Foment install script
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

#!nounbound
(import (rnrs)
	(sagittarius)
	(sagittarius process)
	(tools)
	(util file)
	(srfi :39))

(define (install version)
  (define real-version (or version "master"))
  (define (download)
    (let ((b (scheme-env:download-github-archive
	      (format "/leftmike/foment/archive/~a.zip" real-version))))
      (scheme-env:extract-archive-port (open-bytevector-input-port b) 'zip)))
  (define install-dir (scheme-env:installation-path "foment" real-version))
  (scheme-env:with-work-directory "foment" real-version
    (lambda (work-dir)
      (let ()
	(download)
	(let ((path (scheme-env:find-extracted-directory ".")))
	  (parameterize ((current-directory (build-path path "unix")))
	    (run "make")
	    (unless (file-exists? install-dir)
	      (create-directory* install-dir))
	    (copy-file "release/foment"
		       (build-path install-dir "foment")))))))
  (let ((new (scheme-env:binary-path "foment" real-version))
	(old (build-path* install-dir "foment")))
    (when (file-exists? new) (delete-file new))
    (create-symbolic-link old new)
    (scheme-env:finish-message "Foment" real-version)))
