#lang racket/base
;; Point structures and functions.
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

(provide (struct-out point)
	 (struct-out point-median)
	 point->hash
	 point+
	 point-
	 point*
	 point-copy
	 midpoint
	 point-magnitude
	 point-normalize
	 point-rotate
	 point-rotate-around)

(require racket/match)
(require "main.rkt")

(struct point ([x #:mutable]
	       [y #:mutable])
	#:transparent
	#:methods gen:constrainable
	[(define (constrainable-value-ref self key)
	   (match-define (point x y) self)
	   (match key
	     ['x x]
	     ['y y]))
	 (define (apply-constraint-delta! self key delta)
	   (match key
	     ['x (set-point-x! self (+ (point-x self) delta))]
	     ['y (set-point-y! self (+ (point-y self) delta))]))])

(define (point->hash p)
  (hash 'x (@ p x)
	'y (@ p y)))

(define (point+ a b)
  (point (+ (@ a x) (@ b x))
	 (+ (@ a y) (@ b y))))

(define (point- a b)
  (point (- (@ a x) (@ b x))
	 (- (@ a y) (@ b y))))

(define (point* scalar p)
  (point (* scalar (@ p x))
	 (* scalar (@ p y))))

(define (point-copy p)
  (point (@ p x) (@ p y)))

(define (midpoint a b)
  (point (* 0.5 (+ (@ a x) (@ b x)))
	 (* 0.5 (+ (@ a y) (@ b y)))))

(define (point-magnitude p)
  (define x (@ p x))
  (define y (@ p y))
  (sqrt (+ (* x x) (* y y))))

(define (point-normalize p)
  (define m (point-magnitude p))
  (if (zero? m)
      (point 1 0)
      (point* (/ m) p)))

(define (point-rotate p theta)
  (define c (cos theta))
  (define s (sin theta))
  (define x (@ p x))
  (define y (@ p y))
  (point (- (* c x) (* s y))
	 (+ (* s x) (* c y))))

(define (point-rotate-around p theta axis)
  (point+ axis
	  (point-rotate (point- p axis)
			theta)))

(struct point-median (a b)
	#:transparent
	#:methods gen:constrainable
	[(define (constrainable-value-ref self key)
	   (match-define (point-median a b) self)
	   (match key
	     ['x (@ (midpoint a b) x)]
	     ['y (@ (midpoint a b) y)]))
	 (define (apply-constraint-delta! self key delta)
	   (apply-constraint-delta! (point-median-a self) key delta)
	   (apply-constraint-delta! (point-median-b self) key delta))])
