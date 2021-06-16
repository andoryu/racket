#lang racket/gui
(require rackunit "common.rkt")
(require rackunit "nn3.rkt")

(define cities (city-locations 35 100))

(define city-distances (distances (first cities) (rest cities)))

  (define (ordered-city-index ordered-city)
    (vector-ref (car ordered-city) 0)
    )

(list-ref city-distances 0)
(ordered-city-index (list-ref city-distances 0))
(sort-by-distance city-distances)

(define n3 (nearest3 (first cities) (rest cities) 3))

(define test (nn3-path cities))