;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; host-update.scm - Scheme environment host-update command script
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
	(sagittarius process)
	(util file)
	(tools)
	(srfi :13)
	(srfi :14)
	(srfi :18))

(define (usage)
  (scheme-env:message "scheme-env host-update path-or-name")
  (scheme-env:message " Updates the host Scheme with given path")
  (exit -1))

(define (resolve-path path)
  (cond ((and (file-exists? path) (absolute-path? path)) path) ;; let's check
	((and (string-contains path "@") (string-prefix? "sagittarius" path))
	 (let ((link (build-path (scheme-env-bin-directory) path)))
	   (and (file-exists? link) link)))
	(else #f)))
	 

(define (update path)
  (define real-path (resolve-path path))
  (define (reader process stdout stderr transcoder)
    (define (make-task in out)
      (define tin (transcoded-port in (native-transcoder)))
      (lambda ()
	(let loop ()
	  (if (port-ready? tin)
	      (let ((line (get-line tin)))
		(put-string out line))
	      (begin
		(thread-sleep! 0.1)
		(loop))))))
    (let ((in (process-output-port process)))
      (values process (thread-start! (make-thread (make-task in stdout))))))
  (define (compare-version new old)
    (let loop ((new (map string->number new)) (old (map string->number old)))
      (cond ((and (null? new) (null? old)) #t)
	    ((null? new) #f)
	    ((null? old) #t) ;; 4 digits
	    ((> (car new) (car old)) #t)
	    ((= (car new) (car old)) (loop (cdr new) (cdr old)))
	    (else #f))))
  (unless real-path
    (assertion-violation 'host-update "The given path or name doesn't exist"
			 path))
  (scheme-env:message "Checking version ... ")
  (let*-values (((out extract) (open-string-output-port))
		((p thread)
		 (create-process real-path
				 '("-e" "(print (sagittarius-version)) (exit)")
				 :stdout out
				 :reader reader)))
    (thread-join! thread)
    (scheme-env:print "done!")
    (let* ((version (extract))
	   (new-version (string-tokenize version char-set:digit))
	   (old-version (string-tokenize (sagittarius-version) char-set:digit)))
      (scheme-env:print (format "New version ... ~a" version))
      ;; must be newer version
      (if (compare-version new-version old-version)
	  (let ((host (scheme-env-host-implementation)))
	    (when (file-exists? host) (delete-file host))
	    (create-symbolic-link real-path host)
	    (scheme-env:print "Host Scheme is updated"))
	  (scheme-env:print "Specified Sagittarius older than current")))))

(define (main args)
  (when (null? args) (usage))
  (update (car args)))
