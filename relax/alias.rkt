#lang racket/base
;; Map between constrainable fields.
;;
;; Copyright (c) 2014 Communications Design Group / SAP Labs.
;; Copyright (c) 2014 Tony Garnock-Jones <tonygarnockjones@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(provide (struct-out constrainable-alias)
	 alias)

(require racket/match)
(require racket/generic)
(require "main.rkt")

(define (complain-key-not-found self key action)
  (error 'alias "Key not found when ~a: ~v (available keys: ~v)"
	 action
	 key
	 (map car (constrainable-alias-mapping self))))

(struct constrainable-alias (mapping)
	#:transparent
	#:methods gen:constrainable
	[(define/generic super-value constrainable-value-ref)
	 (define/generic super-apply apply-constraint-delta!)
	 (define (constrainable-value-ref self key)
	   (match (assq key (constrainable-alias-mapping self))
	     [(list _ target target-key) (super-value target target-key)]
	     [#f (complain-key-not-found self key 'reading)]))
	 (define (apply-constraint-delta! self key delta)
	   (match (assq key (constrainable-alias-mapping self))
	     [(list _ target target-key) (super-apply target target-key delta)]
	     [#f (complain-key-not-found self key 'updating)]))])

(define-syntax-rule (alias [key target target-key] ...)
  (constrainable-alias (list (list 'key target 'target-key) ...)))
