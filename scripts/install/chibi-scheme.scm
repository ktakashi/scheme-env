;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; install/chibi-scheme.scm - Chibi Scheme install script
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
	(srfi :39))

(define (install version)
  (define real-version (or version "master"))
  (define install-prefix
    (scheme-env:installation-path "chibi-scheme" real-version))
  (define (download)
    (let ((b (scheme-env:download-github-archive
	      (format "/ashinn/chibi-scheme/archive/~a.zip" real-version))))
      (scheme-env:extract-archive-port (open-bytevector-input-port b) 'zip)))
  (scheme-env:with-work-directory "chibi-scheme" real-version
    (lambda (work-dir)
      (download)
      (let ((path (scheme-env:find-extracted-directory "."))
	    (prefix (format "PREFIX=~a" install-prefix)))
	(parameterize ((current-directory path))
	  (run "make" prefix)
	  (run "make" prefix "install")))))
  (let ((new (scheme-env:binary-path "chibi-scheme" real-version)))
    (scheme-env:create-script-file new install-prefix
				   "chibi-scheme" "bin" "lib")
    (scheme-env:finish-message "Chibi Scheme" real-version)))
