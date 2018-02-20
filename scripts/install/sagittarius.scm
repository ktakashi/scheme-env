;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; install/sagittarius.scm - Sagittarius Scheme install script
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
	(rfc http)
	(util file)
	(tools)
	(srfi :13)
	(srfi :39))

(define-constant +butbucket+ "bitbucket.org")
(define (get-latest-version)
  (define version.txt
    "/ktakashi/sagittarius-scheme/downloads/latest-version.txt")
  (let-values (((s h b) (http-get +butbucket+ version.txt :secure #t)))
    (string-trim-both b)))

(define (install version)
  (define real-version (or version (get-latest-version)))
  (define install-prefix
    (build-path* (scheme-env-implentations-directory)
		 "sagittarius" real-version))
  (define (download-head)
    (let ((b (scheme-env:download-github-archive
	      "/ktakashi/sagittarius-scheme/archive/master.zip")))
      (scheme-env:extract-archive-port (open-bytevector-input-port b) 'zip)))
  (define (download-version version)
    (define path
      (format "/ktakashi/sagittarius-scheme/downloads/sagittarius-~a.tar.gz"
	      version))
    (define file "sagittarius.tar.gz")
    (let-values (((s h b)
		  (http-get +butbucket+ path
			    :receiver (http-file-receiver file)
			    :secure #t)))
      (unless (string=? s "200")
	(assertion-violation 'sagittarius
			     "Failed to download Sagittarius Scheme" s h))
      (call-with-input-file file
	(lambda (in) (scheme-env:extract-archive-port in 'tar.gz))
	:transcoder #f)))
  (define (download)
    (cond ((equal? real-version "head") (download-head))
	  (else (download-version real-version))))
  (define (run-dist)
    (when (equal? real-version "head")
      (run "env" (format "SASH=~a/bin/host-scheme" (scheme-env-home))
	   "sh" "dist.sh" "gen")))
  (scheme-env:with-work-directory "sagittarius" real-version
    (lambda (work-dir)
      (download)
      (let ((path (scheme-env:find-extracted-directory "."))
	    (prefix (format "-DCMAKE_INSTALL_PREFIX=~a" install-prefix)))
	(parameterize ((current-directory path))
	  (run-dist)
	  (run "cmake" prefix ".")
	  (run "make")
	  (run "make" "install")))))
  (let ((new (scheme-env:binary-path "sagittarius" real-version)))
    (scheme-env:create-script-file new install-prefix "sagittarius" "bin" "lib")
    (scheme-env:finish-message "Sagittarius Scheme" real-version)))
