#lang racket/gui

(require list-util)

(require "common.rkt")
 
(define (city-distances city cities)
  (for/list ([i cities])
    (vector i (calc-distance city i)
    )))

(define (city-sort a b)
  (cond
    [(< (vector-ref a 1) (vector-ref b 1)) #t]
    [else #f]))

(define (order-by-distance city cities)
  (let ([distances (city-distances city cities)])
    (for/list ([i (sort distances city-sort)])
        (vector-ref i 0))))

(define (nearest-neighbour cities)
  (cond
    [(empty? cities) empty]
    [else (cons (first cities)
                (nearest-neighbour (order-by-distance (first cities)
                                                      (rest cities))))]))

(define (nn-path cities)
  (let ([path (append (nearest-neighbour cities) (list (first cities)))])
    (zip path (rest path))))

(provide nn-path)
