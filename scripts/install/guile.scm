;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; install/guile.scm - Guile Scheme install script
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
#!read-macro=sagittarius/regex
(import (rnrs)
	(sagittarius)
	(sagittarius process)
	(sagittarius regex)
	(tools)
	(srfi :39))

(define (install version)
  (define real-version
    (scheme-env:call-with-ftp-connection "ftp.gnu.org" "/gnu/guile/"
     (lambda (ftp-conn)
       (or version (scheme-env:latest-version-from-ftp
		    ftp-conn #/guile-(\d\.\d\.\d).tar.gz/
		    scheme-env:semantic-version>?)))))
  (define install-prefix
    (scheme-env:installation-path "guile" real-version))
  (define (download)
    (define file (format "/gnu/guile/guile-~a.tar.gz" real-version))
    (scheme-env:print "Downloading " file)
    (let ((b (scheme-env:download-archive "ftp.gnu.org" file
					  :secure #t)))
      (scheme-env:extract-archive-port (open-bytevector-input-port b) 'tar.gz)))
  (scheme-env:with-work-directory "guile" real-version
   (lambda (work-dir)
     (download)
     (let ((path (scheme-env:find-extracted-directory ".")))
       (parameterize ((current-directory path))
	 (run "sh" "configure" (format "--prefix=~a" install-prefix))
	 (run "make")
	 (run "make" "install")))))
  (let ((new (scheme-env:binary-path "guile" real-version)))
    (scheme-env:call-with-script-file new install-prefix "guile"
      (lambda (out) (format out "~a/bin/guile \"$@\"~%" install-prefix)))
    (scheme-env:finish-message "Guile Scheme" real-version)))
