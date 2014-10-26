#lang racket/base
;; Geometric constraints.
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

(provide (all-from-out "point.rkt")
	 (struct-out prioritize-constraint)
	 (struct-out fixed-coordinate-constraint)
	 (struct-out coincidence-constraint)
	 (struct-out length-constraint)
	 (struct-out difference-constraint)
	 (struct-out orientation-constraint)
	 (struct-out heading-constraint))

(require racket/generic)
(require racket/match)
(require (only-in racket/math pi))
(require "main.rkt")
(require "point.rkt")

(struct prioritize-constraint (priority c) #:transparent
	#:methods gen:constraint
	[(define/generic super-compute compute-constraint-deltas)
	 (define (constraint-priority self)
	   (prioritize-constraint-priority self))
	 (define (compute-constraint-deltas self t)
	   (super-compute (prioritize-constraint-c self) t))])

(struct fixed-coordinate-constraint (target source) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (fixed-coordinate-constraint target source) self)
	   (hash target
		 (hash 'x (- (@ source x) (@ target x))
		       'y (- (@ source y) (@ target y)))))])

(struct coincidence-constraint (p1 p2) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (coincidence-constraint p1 p2) self)
	   (define split-difference (point* 0.5 (point- p2 p1)))
	   (hash p1 (point->hash split-difference)
		 p2 (point->hash (point* -1 split-difference))))])

(struct length-constraint (p1 p2 l) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (length-constraint p1 p2 l) self)
	   (define l12 (point-magnitude (point- p1 p2)))
	   (define delta (/ (- l12 l) 2))
	   (define e12 (point* delta (point-normalize (point- p2 p1))))
	   (hash p1 (point->hash e12)
		 p2 (point->hash (point* -1 e12))))])

(struct difference-constraint (b1 b2 l) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (difference-constraint b1 b2 l) self)
	   (define dist (abs (- (@ b2 value) (@ b1 value))))
	   (define delta (/ (- dist (@ l value)) 2))
	   (hash b1 (hash 'value delta)
		 b2 (hash 'value (- delta))))])

(define (safe-atan2 y x)
  (if (and (zero? y) (zero? x))
      0
      (atan y x)))

(define (normalize-angle a)
  (if (negative? a)
      (+ a (* 2 pi))
      a))

(struct orientation-constraint (p1 p2 p3 p4 theta) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (orientation-constraint p1 p2 p3 p4 theta) self)
	   (define v12 (point- p2 p1))
	   (define a12 (safe-atan2 (point-y v12) (point-x v12)))
	   (define m12 (point-median p1 p2))
	   (define v34 (point- p4 p3))
	   (define a34 (safe-atan2 (point-y v34) (point-x v34)))
	   (define m34 (point-median p3 p4))
	   (define current-theta (- (normalize-angle a12) (normalize-angle a34)))
	   (define d-theta (- (normalize-angle theta) (normalize-angle current-theta)))
	   (hash p1 (point->hash (point- (point-rotate-around p1 d-theta m12) p1))
		 p2 (point->hash (point- (point-rotate-around p2 d-theta m12) p2))
		 p3 (point->hash (point- (point-rotate-around p3 (- d-theta) m34) p3))
		 p4 (point->hash (point- (point-rotate-around p4 (- d-theta) m34) p4))))])

(struct heading-constraint (p1 p2 theta) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (heading-constraint p1 p2 theta) self)
	   (define v12 (point- p2 p1))
	   (define a12 (safe-atan2 (point-y v12) (point-x v12)))
	   (define m12 (point-median p1 p2))
	   (define d-theta (- (normalize-angle theta) (normalize-angle a12)))
	   (hash p1 (point->hash (point- (point-rotate-around p1 d-theta m12) p1))
		 p2 (point->hash (point- (point-rotate-around p2 d-theta m12) p2))))])
