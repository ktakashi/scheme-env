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
#!nounbound
(library (tools)
  (export scheme-env-repository
	  scheme-env-home
	  scheme-env-bin-directory
	  scheme-env-sitelib-directory
	  scheme-env-metainf-directory
	  scheme-env-work-directory
	  scheme-env-implentations-directory
	  scheme-env-tmp-directory
	  scheme-env-default-implementation
	  scheme-env-host-implementation

	  scheme-env:download
	  scheme-env:script-file

	  scheme-env:parse-version
	  
	  scheme-env:with-work-directory

	  scheme-env:call-with-ftp-connection

	  scheme-env:latest-version-from-ftp
	  scheme-env:download-ftp-archive
	  scheme-env:semantic-version-compare
	  scheme-env:semantic-version<?
	  scheme-env:semantic-version>?

	  scheme-env:download-archive
	  scheme-env:download-github-archive
	  scheme-env:github-latest-version
	  scheme-env:extract-archive-port
	  scheme-env:find-extracted-directory
	  scheme-env:installation-path
	  scheme-env:binary-path
	  scheme-env:create-script-file scheme-env:create-script-file/env
	  scheme-env:call-with-script-file
	  scheme-env:finish-message
	  scheme-env:message
	  scheme-env:print

	  ;; conditions
	  scheme-env-error
	  scheme-env-condition?

	  scheme-env-command-not-found?
	  scheme-env-condition-command

	  scheme-env-download-condition?
	  scheme-env-condition-host
	  scheme-env-condition-path

	  scheme-env-github-condition?
	  )
  (import (rnrs)
	  (sagittarius)
	  (sagittarius regex)
	  (archive)
	  (util file)
	  (rfc ftp)
	  (rfc http)
	  (rfc gzip)
	  (srfi :1)
	  (srfi :26)
	  (srfi :39))
(define-constant +default-github-repository+
  "https://raw.githubusercontent.com/ktakashi/scheme-env/master")

(define-condition-type &scheme-env-condition &condition
  make-scheme-env-condition scheme-env-condition?)

(define-condition-type &scheme-env-command-not-found &scheme-env-condition
  make-scheme-env-command-not-found scheme-env-command-not-found?
  (command scheme-env-condition-command))

(define-condition-type &scheme-env-file-not-found &scheme-env-condition
  make-scheme-env-file-not-found scheme-env-file-not-found?
  (file scheme-env-condition-file))

(define-condition-type &scheme-env-download-condition &scheme-env-condition
  make-scheme-env-download-condition scheme-env-download-condition?
  (host scheme-env-condition-host)
  (path scheme-env-condition-path))

(define-condition-type &scheme-env-github-condition &scheme-env-condition
  make-scheme-env-github-condition scheme-env-github-condition?)

(define (scheme-env-error who message . irr)
  (raise (condition
	  (make-error)
	  (make-who-condition who)
	  (make-scheme-env-condition)
	  (make-message-condition message)
	  (make-irritants-condition irr))))

(define (scheme-env-download-error host path message)
  (raise (condition
	  (make-error)
	  (make-who-condition 'scheme-env)
	  (make-scheme-env-download-condition host path)
	  (make-message-condition message))))

(define (scheme-env-github-error message)
  (raise (condition
	  (make-error)
	  (make-who-condition 'scheme-env)
	  (make-scheme-env-github-condition)
	  (make-message-condition message))))

(define (scheme-env-command-not-found-error command message)
  (raise (condition
	  (make-error)
	  (make-who-condition 'scheme-env)
	  (make-scheme-env-command-not-found command)
	  (make-message-condition message))))
(define (scheme-env-file-not-found-error file message)
  (raise (condition
	  (make-error)
	  (make-who-condition 'scheme-env)
	  (make-scheme-env-file-not-found file)
	  (make-message-condition message))))

(define (scheme-env-home)
  (or (getenv "SCHEME_ENV_HOME")
      (scheme-env-error 'scheme-env-home "SCHEME_ENV_HOME is not set")))
(define (scheme-env-repository)
  (cond ((getenv "SCHEME_ENV_REPOSITORY"))
	(else +default-github-repository+)))
(define (scheme-env-bin-directory) (build-path (scheme-env-home) "bin"))
(define (scheme-env-sitelib-directory) (build-path (scheme-env-home) "sitelib"))
(define (scheme-env-metainf-directory) (build-path (scheme-env-home) "metainf"))
(define (scheme-env-work-directory) (build-path (scheme-env-home) "work"))
(define (scheme-env-tmp-directory) (build-path (scheme-env-home) "tmp"))
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
	(scheme-env-file-not-found-error file "file not found"))
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
		  (scheme-env-file-not-found-error file
		    "File not found in specified repository")))))))

(define (->scheme-file pre part) (format "~a/~a.scm" pre part))
(define (scheme-env:script-file command)
  (guard (e (else
	     (scheme-env-command-not-found-error command "no such command")))
    (scheme-env:download (->scheme-file "scripts" command))))

(define (scheme-env:with-work-directory name version proc)
  (let ((work-dir (build-path* (scheme-env-work-directory) name version)))
    (when (file-exists? work-dir) (delete-directory* work-dir))
    (create-directory* work-dir)
    (parameterize ((current-directory work-dir)) (proc work-dir))))

(define (scheme-env:call-with-ftp-connection host path proc . args)
  (define ftp-conn (apply ftp-login host args))
  (ftp-chdir ftp-conn path)
  (guard (e (else (ftp-quit ftp-conn) (raise e)))
    (let ((r (proc ftp-conn)))
      (ftp-quit ftp-conn)
      r)))

(define (scheme-env:latest-version-from-ftp ftp-conn version-pattern compare)
  (define (extract-version name)
    (cond ((version-pattern name) => (lambda (m) (m 1)))
	  (else #f)))
  
  (let ((r (filter-map extract-version (ftp-name-list ftp-conn))))
    (when (null? r)
      (scheme-env-error 'scheme-env:version-from-ftp
			"Couldn't determine the latest version from names"
			version-pattern))
    (car (list-sort compare r))))

(define (scheme-env:download-ftp-archive ftp-conn file)
  (ftp-get ftp-conn file))

;; semantic compare
(define (scheme-env:semantic-version-compare v1 v2)
  (let loop ((v1* (string-split v1 "\\."))
	     (v2* (string-split v2 "\\.")))
    (cond ((and (null? v1*) (null? v2*)) 0)
	  ((null? v1*) -1)
	  ((null? v2*) 1)
	  (else
	   (let ((r (compare (string->number (car v1*))
			     (string->number (car v2*)))))
	     (if (zero? r)
		 (loop (cdr v1*) (cdr v2*))
		 r))))))

(define (scheme-env:semantic-version<? v1 v2)
  (< (scheme-env:semantic-version-compare v1 v2) 0))
(define (scheme-env:semantic-version>? v1 v2)
  (> (scheme-env:semantic-version-compare v1 v2) 0))

;; separated by @
;; name@version -> (values name version)
;; name -> (values name #f)
(define (scheme-env:parse-version arg)
  (cond ((#/([^@]+)@(.+)/ arg) => (lambda (m) (values (m 1) (m 2))))
	(else (values arg #f))))

(define (scheme-env:download-archive host path
	  :key (receiver (http-binary-receiver)) :allow-other-keys opt)
  (let-values (((s h b) (apply http-get host path :receiver receiver opt)))
    (unless (string=? s "200")
      (scheme-env-download-error host path "Failed to download"))
    b))

(define (scheme-env:download-github-archive path . opt)
  (apply scheme-env:download-archive "github.com" path :secure #t opt))

(define (scheme-env:github-latest-version path)
  (define (err msg) (scheme-env-github-error msg))
  (define (parse-version url)
    (cond ((#/tag\/(.+?)$/ url) => (lambda (m) (m 1)))
	  (else (err "Redirecting URL doesn't contain valid path"))))
  
  (let-values (((s h c)
		(http-head "github.com" (format "~a/releases/latest" path)
			   :secure #t :no-redirect #t)))
    (if (string=? s "302")
	(cond ((assoc "location" h) =>
	       (lambda (slot) (parse-version (cadr slot))))
	      (else (err "No Location header")))
	(err "Invalid Http status code"))))

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

(define (scheme-env:installation-path name version)
  (build-path* (scheme-env-implentations-directory) name version))

(define (scheme-env:binary-path name version)
  (build-path* (scheme-env-home) "bin" (format "~a@~a" name version)))

(define *ld-library-path-name*
  (cond-expand
   (darwin "DYLD_LIBRARY_PATH")
   (else "LD_LIBRARY_PATH")))

(define (scheme-env:create-script-file binary-path prefix name bin lib)
  (scheme-env:call-with-script-file binary-path prefix name
    (lambda (out)
      (let ((bin (build-path* prefix bin name))
	    (lib (build-path* prefix lib)))
	(format out "exec env ~a=~a:${~a} ~a \"$@\"~%"
		*ld-library-path-name* lib *ld-library-path-name* bin)))))

(define (scheme-env:create-script-file/env binary-path prefix name bin lib env)
  (scheme-env:call-with-script-file binary-path prefix name
    (lambda (out)
      (let ((bin (build-path* prefix bin name))
	    (lib (build-path* prefix lib)))
	(format out "exec env ~a=~a:${~a} ~a ~a \"$@\"~%"
		*ld-library-path-name* lib *ld-library-path-name* env bin)))))

(define (scheme-env:call-with-script-file binary-path prefix name proc)
  (define (call-with-safe-output-file file proc)
    (when (file-exists? file) (delete-file file))
    (call-with-output-file file proc))
  (let ((new binary-path)
	(script (build-path* prefix name)))
    (when (file-exists? script) (delete-file script))
    (call-with-output-file script
      (lambda (out)
	(put-string out "#!/bin/sh\n")
	(proc out)
	(change-file-mode script #o775)
	(when (file-exists? new) (delete-file new))
	(create-symbolic-link script new)
	script))))

(define (scheme-env:finish-message implementation version)
  (format #t "~a@~a is installed ~%" implementation version))

(define (scheme-env:message message)
  (display message)
  (flush-output-port (current-output-port)))
(define (scheme-env:print . args) (for-each display args) (newline))
)
