#lang racket/base

(define (calc-distance a b)
  (let ([ax (vector-ref a 1)]
        [ay (vector-ref a 2)]
        [bx (vector-ref b 1)]
        [by (vector-ref b 2)]
        )
    (sqrt (+
           (expt (- ax bx) 2)
           (expt (- ay by) 2)))
  ))

(provide calc-distance)