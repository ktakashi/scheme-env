;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; install/larceny - Larceny install script
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

(define-constant +larcenists+ "www.larcenists.org")
(define (ask-version)
  (display "Please entire the version (e.g. 1.3)") (newline)
  (display "> ") (flush-output-port)
  (get-line (current-input-port)))

(define (install version)
  (define real-version (or version (ask-version)))
  (define install-prefix
    (scheme-env:installation-path "larceny" real-version))
  (define (download platform)
    (let ((b (scheme-env:download-archive +larcenists+
	       (format "/LarcenyReleases/larceny-~a-bin-native-ia32-~a.tar.gz"
		       real-version platform))))
      (scheme-env:extract-archive-port (open-bytevector-input-port b) 'tar.gz)))
  (scheme-env:with-work-directory "larceny" real-version
    (lambda (work-dir)
      ;; FIXME platform
      (download "linux86")
      (let ((path (scheme-env:find-extracted-directory ".")))
	(delete-directory* install-prefix)
	(rename-file path install-prefix))))
  (let ((new (scheme-env:binary-path "larceny" real-version)))
    (scheme-env:call-with-script-file new install-prefix "run-larceny"
      (lambda (out)
	(format out "LARCENY_ROOT=~a sh ~a/larceny \"$@\"~%"
		install-prefix
		install-prefix)))
    (scheme-env:finish-message "Larceny" real-version)))

