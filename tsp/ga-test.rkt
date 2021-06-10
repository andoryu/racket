#lang racket/gui

(require rackunit "ga.rkt")

(define (test-city-locations count)
  (build-list count (lambda (x)
                      (vector x (* x 2) (* x 2)))))

; city list builder - mocked for testing but needs to be right, right?
(define cities (test-city-locations 20))
(check-equal? (length cities) 20 "number of cities")
(check-equal? (list-ref cities 2) #(2 4 4) "city data check 1")
(check-equal? (list-ref cities 19) #(19 38 38) "city data check 2")

; population creation - includes shuffle so hard to validate
(define test-pop (create-population 100 20))
(check-equal? (length test-pop) 100 "population length")
(check-equal? (length (list-ref test-pop 1)) 21 "agent length")

;check calc-distance
(define distances (calc-distances test-pop cities))
;(list-ref distances 0)
(check-equal? (length distances) 100 "distances length")
(check-equal? (length (car (list-ref distances 1))) 21 "distances agent length")
(check-true (number? (cdr (list-ref distances 1))) "distances agent distance calc")

;check sorting
(define sorted-pop (sort-asc distances))
(check-true (< (cdr (list-ref sorted-pop 0)) (cdr (list-ref sorted-pop 99))))

;check creation of the roulette reproduction rule
(check-equal? (length (take sorted-pop 10)) 10)
(define weights (roulette-weights (take sorted-pop 10)))
(check-equal? (length weights) 10 "roulette weights length")
(define wheel (roulette-wheel (take sorted-pop 10)))
;run a generation cycle
(define new-gen (generation-cycle sorted-pop))
(check-equal? (length new-gen) 100)

(define a (list 0 1 2 3 4 5 6 7 8 9 10 0))
(define b (list 0 10 9 8 7 6 5 4 3 2 1 0))
a
(perform-cross-over a b 2 4)
;(cross-over a b)