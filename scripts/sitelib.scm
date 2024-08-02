;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; install.scm - Scheme environment install command script
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
	(rnrs eval)
	(sagittarius)
	(getopt)
	(srfi :1 lists)
	(srfi :19 time)
	(srfi :39 parameters)
	(rfc uri)
	(util file)
	(tools)
	)

(define (usage)
  (define p scheme-env:print)
  (p "scheme-env sitelib [-i $implementation] [-d] $name $source")
  (p)
  (p "Description")
  (p " Installs library to Scheme Env's load path")
  (p)
  (p "$name is a name of the library to be installed / uninstalled")
  (p)
  (p "$source must be a root directory of the installing library")
  (p "The structure of the library directory is kept")
  (p)
  (p "-i,--implementation: Installing implementation specific location")
  (p)
  (p "-d,--delete: Removing the specified library")
  (p "   This option ignores $source")
  (p)
  (p "-c,--catalog: Catalog installation")
  (p "   For this option, $name must be location of the catalog.")
  (p "   This option ignores $source and `-i` option")
  (p)
  (p "`-c` and `-d` options can't be used together")
  (exit -1))

(define (ensure-directory! dir)
  (unless (file-exists? dir) (create-directory* dir)))

(define (check-implementation implementation)
  (when (null? (find-files (scheme-env-bin-directory)
			   :physical #f
			   :pattern (string-append implementation "@.*")
			   :recursive #f))
    (scheme-env:print "ERROR: unknown implementation " implementation)
    (scheme-env:print)
    (usage)))

(define (meta-inf name)
  (define metainf (scheme-env-metainf-directory))
  (build-path* metainf name))

(define (uninstall meta-inf)
  (define (safe-delete file) (when (file-exists? file) (delete-file file)))
  (define (safe-delete-directory file)
    (when (file-exists? file) (delete-directory* file)))
  
  (when (file-exists? meta-inf)
    (let* ((inf (call-with-input-file meta-inf get-datum))
	   (files (cond ((assq 'files inf) => cdr)
			(else '()))))
      (scheme-env:print "Uninstalling " (cond ((assq 'name inf) => cdr)
					      (else "'unknown'")))
      (let-values (((files dirs)
		    (partition (lambda (s) (eq? (car s) 'file)) files)))
	(for-each safe-delete (map cdr files))
	(for-each safe-delete-directory (map cdr dirs))
	(delete-file meta-inf)))))

(define (install meta-inf dest name source)
  (define (mkdir-p dir)
    (when (and dir (not (file-exists? dir))) (create-directory* dir)))
  (define (copy-files dest source)
    (define source-len (string-length source))
    (lambda (path type)
      (define l (string-length path))
      (let ((dest-file (build-path dest (substring path (+ source-len 1) l))))
	(if (eq? type 'directory)
	    (mkdir-p dest-file)
	    (let-values (((dir base ext) (decompose-path dest-file)))
	      (when (and dir (not (file-exists? dir))) (create-directory* dir))
	      (scheme-env:print "Copying '" path "' -> '" dest-file "'")
	      (copy-file path dest-file #t)))
	(cons type dest-file))))
  
  (unless (file-directory? source)
    (scheme-env:print "Source must be a directory: " source)
    (usage))
  (when (file-exists? meta-inf) (uninstall meta-inf))
  (scheme-env:print "Installing '" name "' from '" source "'")
  (let ((files (path-map source (copy-files dest source) :recursive #t))
	(now (current-time)))
  (call-with-output-file meta-inf
    (lambda (out)
      (put-datum out
		 `((name . ,name)
		   (files . ,files)
		   (timestamp (second ,(time-second now))
			      (nanosecond ,(time-second now)))
		   (timestamp-str ,(date->string (time-utc->date now 0)))))))))

(define (process name implementation source)
  (define sitelib (scheme-env-sitelib-directory))
  (when implementation (check-implementation implementation))
  (let ((dest (if implementation
		  (build-path sitelib implementation)
		  sitelib)))
    (ensure-directory! dest)
    (ensure-directory! (scheme-env-metainf-directory))
    (let ((metainf (meta-inf name)))
      (install metainf dest name source))))

#|
Catalog format
(
  (name "$name")
  (implementation "$implementation") ;; optional
  (source "$source-uri")
  (archive "$source-archive-uri")
)
|#
(define (call-with-catalog catalog proc)
  (define (no-default key)
    (assertion-violation 'scheme-env-catalog
			 "Missing required catalog key" key))
  (define (assoc-ref key lis :optional (default-value no-default))
    (cond ((assq key lis) => cadr)
	  (else (default-value key))))
  (define (extract uri)
    (define (detect-type uri)
      (let*-values (((s u h p path q f) (uri-parse uri))
		    ((ext) (path-extension path)))
	(unless ext
	  (assertion-violation 'scheme-env-catalog
	   "archive must contain a URI with archive format" uri))
	(string->symbol ext)))
    (scheme-env:call-with-input-uri uri
     (lambda (p)
       (scheme-env:extract-archive-port p (detect-type uri)))))
  (let ((info (scheme-env:call-with-input-uri catalog 
	       (lambda (p) 
		 (get-datum (transcoded-port p (native-transcoder)))))))
    (let ((name (assoc-ref 'name info)))
      (scheme-env:with-work-directory name "dummy"
       (lambda (work-dir)
	 (extract (assoc-ref 'archive info))
	 (let ((d (scheme-env:find-extracted-directory ".")))
	   (parameterize ((current-directory d))
	     (proc name
		   (assoc-ref 'implementation info (lambda _ #f))
		   (assoc-ref 'source info)))))))))

(define (main args)
  (with-args args
      ((implementation (#\i "implementation") #t #f)
       (delete?        (#\d "delete")         #f #f)
       (catalog?       (#\c "catalog")        #f #f)
       . rest)
    (when (or (null? rest)) (usage))
    (when (and delete? catalog?) (usage))
    (cond (delete? (uninstall (meta-inf (car rest))))
	  (catalog? (call-with-catalog (car rest) process))
	  (else
	   (when (null? (cdr rest)) (usage))
	   (process (car rest) implementation (cadr rest))))))
    
