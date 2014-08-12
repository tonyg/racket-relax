#lang racket/base

(provide (all-from-out "point.rkt")
	 (struct-out prioritize-constraint)
	 (struct-out fixed-coordinate-constraint)
	 (struct-out coincidence-constraint)
	 (struct-out length-constraint))

(require racket/match)
(require "main.rkt")
(require "point.rkt")

(struct prioritize-constraint (priority c) #:transparent
	#:methods gen:constraint
	[(define (constraint-priority self)
	   (prioritize-constraint-priority self))
	 (define (compute-constraint-deltas self t)
	   (compute-constraint-deltas (prioritize-constraint-c self) t))])

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
