#lang racket/gui

(require list-util)

(require "common.rkt")

(provide (all-defined-out))

(define (nn3-path cities)
  (define nn3-tree (nearest3 (first cities) (rest cities) 3))
  (for ([c nn3-tree])
    (printf "~a\n" c)
    )

  (printf "~a : ~a : ~a\n"
          (list-ref (list-ref nn3-tree 0) 0)
          (list-ref (list-ref nn3-tree 0) 1)
          (list-ref (list-ref nn3-tree 0) 2)
          )
  
  empty)

(define (nearest3 city cities level)
  (define (ordered-city-index ordered-city)
    (vector-ref ordered-city 0)
    )
  (cond
    [(empty? cities) empty]
    [(zero? level) empty]
    [else
     (define ordered-cities (sort-by-distance (distances city cities)))
     (for/list ([oc (take ordered-cities 3)])
       (cons (ordered-city-index oc)
             (nearest3 oc
                       (remove oc ordered-cities)
                       (sub1 level)))
  
       )
     ]
    )
  )

(define (sort-by-distance cities-distances)
  (map car
       (sort cities-distances city-sort)
       )
  )

(define (city-sort a b)
  (if (< (cdr a) (cdr b)) #t #f)
  )

(define (distances city cities)
  (for/list ([c cities])
    (cons c (calc-distance city c))
    )
  )