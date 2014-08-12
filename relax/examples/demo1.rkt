#lang racket

(require "../main.rkt")
(require "../geometric.rkt")

(define p1 (point 0 0))
(define p2 (point 200 0))

(time (solve-constraints/timeout! (list (length-constraint p1 p2 100))
				  1000
				  #:epsilon 0.0000001))

p1
p2
