#lang racket/base

(provide (struct-out point)
	 point->hash
	 point+
	 point-
	 point*
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

(define (midpoint a b)
  (point (* 0.5 (+ (@ a x) (@ b x)))
	 (* 0.5 (+ (@ a y) (@ b y)))))

(define (point-magnitude p)
  (define x (@ p x))
  (define y (@ p y))
  (sqrt (+ (* x x) (* y y))))

(define (point-normalize p)
  (point* (/ (point-magnitude p)) p))

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
