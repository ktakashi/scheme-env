;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; run.scm - Scheme environment run command script
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
	(scheme load)
	(sagittarius ffi)
	(util file)
	(tools)
	(srfi :0)
	(srfi :13))

(define libc
  (open-shared-library
   (cond-expand
    (cygwin "cygwin1.dll")
    (darwin "libSystem.dylib")
    (32bit "libc.so")
    (else "libc.so.6"))))

(define exec (c-function libc int execv (void* void*)))
(define default (build-path (scheme-env-bin-directory) "default"))

(define (get-implementation/default name)
  (let ((path (build-path (scheme-env-bin-directory) name)))
    (if (file-exists? path)
	path
	default)))

(define (->pointer path converted passing)
  (if converted
      ;; it's rather weird but okay.
      (let* ((tokens (string-tokenize converted))
	     (args (list->vector (append tokens passing)))
	     (len (vector-length args)))
	(do ((i 0 (+ i 1))
	     (array (allocate-pointer (* size-of-void* (+ len 2)))))
	    ((= i len)
	     (pointer-set-c-pointer! array 0 path)
	     array)
	  (pointer-set-c-pointer! array (* (+ i 1) size-of-void*)
				  (vector-ref args i))))
      (let ((array (allocate-pointer (* size-of-void* 2))))
	(pointer-set-c-pointer! array 0 path)
	array)))

(define (usage)
  (define p scheme-env:print)
  (p "scheme-env run [implementation] [options ...]")
  (p)
  (p "Description")
  (p " Executes the [implementation] with the given [options ...].")
  (p)
  (p " If [implementation] is omit, then the default implementation is used")
  (p " and raw [options ...] will be passed")
  (p)
  (p " If [implementation] is provided, then [options ...] can also be the")
  (p " following generic options:")
  (p)
  (p " -l,--loadpath <loadpath>")
  (p "     Adding load path, this option can be specified multiple times")
  (p " -p,--program  <script>")
  (p "     Executes the <script>")
  (p " -r,--standard  [r6rs|r7rs]")
  (p "      Scheme standard to be followed"))

(define (main args)
  (define env (environment '(only (sagittarius) import library define-library)))
  (define (parse-command-line maybe-name args)
    (if maybe-name
	(eval `(convert-command-line ',args) env)
	(values #f '())))
  (define command-line (scheme-env:script-file "command-line"))
  (let* ((maybe-name (and (not (null? args)) (car args)))
	 (path (get-implementation/default (or maybe-name "default"))))
    (load command-line env)
    (let-values (((converted passing) (parse-command-line maybe-name args)))
      (exec path (->pointer path converted passing)))))
