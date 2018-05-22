;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; command-line.scm - Scheme environment command-line command script
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
	(scheme load)
	(getopt)
	(srfi :13)
	(tools))

(define (print . args) (for-each display args) (newline))

;; It seems -I is pretty much common to add load path.
(define (common-converter flags)
  (define (add-script script includes)
    (if script
	(string-append includes " " script)
	includes))
  ;; we ignore the standard flags
  (let ((includes (car flags))
	(script (cadr flags)))
    (add-script script
		(string-join (map (lambda (p) (string-append "-I" p))
				  includes)))))

(define (invoke-converter impl flags)
  (guard (e (else (common-converter flags)))
    (let-values (((impl version) (scheme-env:parse-version impl)))
      (let ((file (scheme-env:script-file (format "command-line/~a" impl)))
	    (env (environment '(only (sagittarius)
				     import library define-library))))
	(load file env)
	(eval `(convert ,impl ,version ',flags) env)))))

;; for some reason this is not in any library...
(define (split-when pred lis)
  (let loop ((lis lis) (r '()))
    (cond ((null? lis) (values (reverse! r) lis))
	  ((pred (car lis)) (values (reverse! r) lis))
	  (else (loop (cdr lis) (cons (car lis) r))))))

;; after -- is for just passing
(define (parse-command-line args)
  (define (check-standard standard)
    (and standard
	 (case (string->symbol standard)
	   ((r6rs) 'r6rs)
	   ((r7rs) 'r7rs)
	   (else (assertion-violation
		  'command-line "only R6RS or R7RS is supported" standard)))))
  (let-values (((flags rest) (split-when (lambda (v) (equal? "--" v)) args)))
    (with-args flags
	((includes (#\l "loadpath") *  '())
	 (program  (#\p "program")  #t #f)
	 (standard (#\r "standard") #t #f)
	 . maybe-file)
      (when (and program (not (null? maybe-file)))
	(assertion-violation 'command-line "--program and loading file can't be specified simultaneously"))
      (let ((file (if (null? maybe-file) #f (car maybe-file))))
	(values `(,includes ,(or program file) ,(check-standard standard))
		(if (null? rest) rest (cdr rest)))))))

(define (main args)
  (when (null? args) (usage))
  (let ((impl (car args)))
    (let-values (((flags passing) (parse-command-line  (cdr args))))
      (let ((converted (if (null? flags) "" (invoke-converter impl flags))))
	(if (null? passing)
	    (print converted)
	    (print converted " " (string-join passing)))))))
