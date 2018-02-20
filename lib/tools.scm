;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; tools.scm - Scheme environment tools
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

#!read-macro=sagittarius/regex
(library (tools)
  (export scheme-env-repository
	  scheme-env-home
	  scheme-env-bin-directory
	  scheme-env-work-directory
	  scheme-env-implentations-directory
	  scheme-env-default-implementation
	  scheme-env-host-implementation

	  scheme-env:download
	  scheme-env:script-file

	  scheme-env:parse-version
	  
	  scheme-env:with-work-directory

	  scheme-env:download-github-archive
	  scheme-env:extract-archive-port
	  scheme-env:find-extracted-directory
	  scheme-env:binary-path
	  scheme-env:create-script-file
	  scheme-env:finish-message
	  )
  (import (rnrs)
	  (sagittarius)
	  (archive)
	  (util file)
	  (rfc http)
	  (srfi :26)
	  (srfi :39))

(define-constant +default-github-repository+
  "https://raw.githubusercontent.com/ktakashi/scheme-env/master")
(define (scheme-env-home)
  (or (getenv "SCHEME_ENV_HOME")
      (assertion-violation 'scheme-env-home "SCHEME_ENV_HOME is not set")))
(define (scheme-env-repository)
  (cond ((getenv "SCHEME_ENV_REPOSITORY"))
	(else +default-github-repository+)))
(define (scheme-env-bin-directory) (build-path (scheme-env-home) "bin"))
(define (scheme-env-work-directory) (build-path (scheme-env-home) "work"))
(define (scheme-env-implentations-directory)
  (build-path (scheme-env-home) "implementations"))
(define (scheme-env-default-implementation)
  (build-path* (scheme-env-home) "bin" "default"))
(define (scheme-env-host-implementation)
  (build-path* (scheme-env-home) "bin" "host-scheme"))

(define (scheme-env:download file)
  (define destination-directory (scheme-env-home))
  (define output-file (build-path destination-directory file))
  (define repository (scheme-env-repository))
  (define (download m)
    (let-values (((s h b)
		  (http-get (m 2) (string-append (m 3) "/" file)
			    :secure (string=? (m 1) "https"))))
      (unless (string=? s "200")
	(assertion-violation 'scheme-env "file not found" file))
      (call-with-output-file output-file
	(lambda (out) (put-string out b)))
      output-file))
  (let-values (((base name ext) (decompose-path output-file)))
    (unless (file-exists? base) (create-directory* base)))
  (cond ((#/(https?):\/\/([^\/]+)(.+)/ repository) =>
	   (lambda (m)
	     (cond ((file-exists? output-file) output-file)
		   (else (download m)))))
	(else
	 (let ((repository-file (build-path repository file)))
	   (cond ((file-exists? repository-file)
		  (copy-file repository-file output-file #t)
		  output-file)
		 ((file-exists? repository-file) repository-file)
		 (else
		  (assertion-violation 'scheme-env:download
				       "File not found in specified repository"
				       file)))))))

(define (->scheme-file pre part) (format "~a/~a.scm" pre part))
(define (scheme-env:script-file command)
  (scheme-env:download (->scheme-file "scripts" command)))

(define (scheme-env:with-work-directory name proc)
  (let ((work-dir (build-path* (scheme-env-work-directory) name)))
    (when (file-exists? work-dir) (delete-directory* work-dir))
    (create-directory* work-dir)
    (parameterize ((current-directory work-dir)) (proc work-dir))))

;; separated by @
;; name@version -> (values name version)
;; name -> (values name #f)
(define (scheme-env:parse-version arg)
  (cond ((#/([^@]+)@(.+)/ arg) => (lambda (m) (values (m 1) (m 2))))
	(else (values arg #f))))

(define (scheme-env:download-github-archive path
	  :key (receiver (http-binary-receiver)))
  (let-values (((s h b) (http-get "github.com" path
				  :receiver receiver :secure #t)))
    (unless (string=? s "200")
      (error 'scheme-env:download-github-archive "Failed to download" path))
    b))

(define (scheme-env:extract-archive-port port type)
  (define (destinator e)
    (let ((name (archive-entry-name e)))
      (format #t "-- Extracting: ~a~%" name)
      name))
  (let-values (((p t) (case type
			((zip) (values port type))
			((tar.gz) (values (open-gzip-input-port port) 'tar)))))
    (call-with-archive-input t p
      (cut extract-all-entries <> :overwrite #t :destinator destinator))))

;; returns first found directory. (should be fine)
(define (scheme-env:find-extracted-directory path)
  (call/cc (lambda (return)
	     (path-for-each path (lambda (p t)
				   (and (eq? t 'directory) (return p)))
			    :recursive #f))))

(define (scheme-env:binary-path name version)
  (build-path* (scheme-env-home) "bin" (format "~a@~a" name version)))

(define (scheme-env:create-script-file binary-path prefix name bin lib)
  (define (call-with-safe-output-file file proc)
    (when (file-exists? file) (delete-file file))
    (call-with-output-file file proc))
  (let ((new binary-path)
	(script (build-path* prefix name))
	(bin (build-path* prefix bin name))
	(lib (build-path* prefix lib)))
    (when (file-exists? script) (delete-file script))
    (call-with-output-file script
      (lambda (out)
	(put-string out "#!/bin/sh\n")
	(format out "LD_LIBRARY_PATH=~a:${LD_LIBRARY_PATH} ~a \"$@\""
		lib bin)
	(change-file-mode script #o775)
	(when (file-exists? new) (delete-file new))
	(create-symbolic-link script new)
	script))))

(define (scheme-env:finish-message implementation version)
  (format #t "~a@~a is installed ~%" implementation version))

)
