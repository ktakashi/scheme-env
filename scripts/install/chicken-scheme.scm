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
	(rfc http)
	(srfi :13)
	(srfi :39))

(define-constant +call-cc.org+ "code.call-cc.org")
(define (get-current-version)
  (let-values (((s h b) (http-get +call-cc.org+ "/releases/current/NEWS")))
    (string-trim-both (get-line (open-string-input-port b)))))

(define (install version)
  (define real-version (or version (get-current-version)))
  (define install-prefix
    (scheme-env:installation-path "chicken-scheme" real-version))
  (define (download)
    (let ((b (scheme-env:download-archive +call-cc.org+
	       (format "/releases/~a/chicken.tar.gz" real-version))))
      (scheme-env:extract-archive-port (open-bytevector-input-port b) 'tar.gz)))
  (scheme-env:with-work-directory "chicken-scheme" real-version
    (lambda (work-dir)
      (download)
      (let ((path (scheme-env:find-extracted-directory "."))
	    (prefix (format "PREFIX=~a" install-prefix))
	    (platform (format "PLATFORM=~a"
			      (cond-expand (darwin "macosx") (else "linux")))))
	(parameterize ((current-directory path))
	  (run "make" prefix platform)
	  (run "make" prefix platform "install")))))
  (let ((new (scheme-env:binary-path "chicken-scheme" real-version))
	(csi (scheme-env:binary-path "csi" real-version))
	(csc (scheme-env:binary-path "csc" real-version)))
    (scheme-env:create-script-file new install-prefix "chicken" "bin" "lib")
    (scheme-env:create-script-file csi install-prefix "csi" "bin" "lib")
    (scheme-env:create-script-file csc install-prefix "csc" "bin" "lib")
    (scheme-env:finish-message "Chicken Scheme" real-version)))
