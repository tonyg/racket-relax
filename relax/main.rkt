#lang racket/base
;; (Geometric/Numeric) Constraint solving by relaxation, after Van Overveld 1993.
;; Following the general design of a Javascript library by Alex Warth.
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

(provide gen:constraint
	 constraint?
	 constraint-priority
	 compute-constraint-deltas

	 gen:constrainable
	 constrainable?
	 constrainable-value-ref
	 apply-constraint-delta!
	 @

	 solve-constraints-one-iteration!
	 solve-constraints/timeout!)

(require (only-in unstable/list group-by))
(require racket/generic)

(define-generics constraint
  (constraint-priority constraint)
  (compute-constraint-deltas constraint wall-clock-time)
  #:fallbacks [(define (constraint-priority c) 0)])

(define-generics constrainable
  (constrainable-value-ref constrainable key)
  (apply-constraint-delta! constrainable key delta))

(define-syntax-rule (@ value key)
  (constrainable-value-ref value 'key))

(define default-epsilon (make-parameter 0.01))
(define default-rho (make-parameter 0.25))

(define (solve-constraints-one-iteration! cs
					  [wall-clock-time (current-inexact-milliseconds)]
					  #:epsilon [epsilon (default-epsilon)]
					  #:rho [rho (default-rho)])
  (for/fold [(did-something? #f)]
      [(constraint-group (sort (group-by constraint-priority cs) <
			       #:key (lambda (g) (constraint-priority (car g)))))]
    (define all-deltas
      (for/fold [(acc '())] [(c constraint-group)]
	(define deltas (compute-constraint-deltas c wall-clock-time))
	(if (and deltas (significant-deltas? deltas epsilon))
	    (cons deltas acc)
	    acc)))
    (if (not (null? all-deltas))
	(begin (for* [(deltas all-deltas)
		      ((var keyvals) (in-hash deltas))
		      ((key val) (in-hash keyvals))]
		 (apply-constraint-delta! var key (* rho val)))
	       #t)
	did-something?)))

(define (solve-constraints/timeout! cs
				    timeout-milliseconds
				    #:epsilon [epsilon (default-epsilon)]
				    #:rho [rho (default-rho)])
  (define start-time (current-inexact-milliseconds))
  (let loop ((now start-time) (iteration-count 0))
    (define did-something? (solve-constraints-one-iteration! cs now #:epsilon epsilon #:rho rho))
    (define next-now (current-inexact-milliseconds))
    (if (and did-something? (< (- next-now start-time) timeout-milliseconds))
	(loop next-now (+ iteration-count 1))
	iteration-count)))

(define (significant-deltas? deltas epsilon)
  (let/ec return
    (for* [((var keyvals) (in-hash deltas))
	   ((key val) (in-hash keyvals))]
      (when (> (abs val) epsilon)
	(return #t)))
    #f))
