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
	 (struct-out fixed-coordinate-constraint)
	 (struct-out coincidence-constraint)
	 (struct-out equivalence-constraint)
	 (struct-out equal-distance-constraint)
	 (struct-out length-constraint)
	 (struct-out point-difference-constraint)
	 (struct-out scalar-difference-constraint)
	 (struct-out orientation-constraint)
	 (struct-out heading-constraint))

(require racket/generic)
(require racket/match)
(require (only-in racket/math pi))
(require "main.rkt")
(require "point.rkt")

(struct fixed-coordinate-constraint (target source) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (fixed-coordinate-constraint target source) self)
	   (constraint-delta [target [x (- (@ source x) (@ target x))]
				     [y (- (@ source y) (@ target y))]]))])

(struct coincidence-constraint (p1 p2) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (coincidence-constraint p1 p2) self)
	   (define split-difference (point* 0.5 (point- p2 p1)))
	   (constraint-delta [p1 <- (point->hash split-difference)]
			     [p2 <- (point->hash (point* -1 split-difference))]))])

(struct equivalence-constraint (p1 p2 p3 p4) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (equivalence-constraint p1 p2 p3 p4) self)
	   (define split-difference (point* 0.25 (point- (point+ p2 p3) (point+ p1 p4))))
	   (define h1 (point->hash split-difference))
	   (define h2 (point->hash (point* -1 split-difference)))
	   (constraint-delta [p1 <- h1] [p2 <- h2] [p3 <- h2] [p4 <- h1]))])

(struct equal-distance-constraint (p1 p2 p3 p4) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (equal-distance-constraint p1 p2 p3 p4) self)
	   (define l12 (point-magnitude (point- p1 p2)))
	   (define l34 (point-magnitude (point- p3 p4)))
	   (define delta (/ (- l12 l34) 4))
	   (define e12 (point* delta (point-normalize (point- p2 p1))))
	   (define e34 (point* delta (point-normalize (point- p4 p3))))
	   (constraint-delta [p1 <- (point->hash e12)]
			     [p2 <- (point->hash (point* -1 e12))]
			     [p3 <- (point->hash (point* -1 e34))]
			     [p4 <- (point->hash e34)]))])

(struct length-constraint (p1 p2 l) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (length-constraint p1 p2 l) self)
	   (define l12 (point-magnitude (point- p1 p2)))
	   (define delta (/ (- l12 l) 2))
	   (define e12 (point* delta (point-normalize (point- p2 p1))))
	   (constraint-delta [p1 <- (point->hash e12)]
			     [p2 <- (point->hash (point* -1 e12))]))])

(struct point-difference-constraint (p1 p2 v) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (point-difference-constraint p1 p2 v) self)
	   (define delta (point* 0.5 (point- (point- p2 p1) v)))
	   (constraint-delta [p1 <- (point->hash delta)]
			     [p2 <- (point->hash (point* -1 delta))]))])

(struct scalar-difference-constraint (b1 b2 l) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (scalar-difference-constraint b1 b2 l) self)
	   (define delta (/ (- (@ b2 value) (@ b1 value) (@ l value)) 2))
	   (constraint-delta [b1 [value delta]]
			     [b2 [value (- delta)]]))])

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
	   (constraint-delta
	    [p1 <- (point->hash (point- (point-rotate-around p1 d-theta m12) p1))]
	    [p2 <- (point->hash (point- (point-rotate-around p2 d-theta m12) p2))]
	    [p3 <- (point->hash (point- (point-rotate-around p3 (- d-theta) m34) p3))]
	    [p4 <- (point->hash (point- (point-rotate-around p4 (- d-theta) m34) p4))]))])

(struct heading-constraint (p1 p2 theta) #:transparent
	#:methods gen:constraint
	[(define (compute-constraint-deltas self t)
	   (match-define (heading-constraint p1 p2 theta) self)
	   (define v12 (point- p2 p1))
	   (define a12 (safe-atan2 (point-y v12) (point-x v12)))
	   (define m12 (point-median p1 p2))
	   (define d-theta (- (normalize-angle theta) (normalize-angle a12)))
	   (constraint-delta
	    [p1 <- (point->hash (point- (point-rotate-around p1 d-theta m12) p1))]
	    [p2 <- (point->hash (point- (point-rotate-around p2 d-theta m12) p2))]))])
