#lang racket/base

(provide calc-distance city-locations)

(define (calc-distance a b)
  (define dx (- (vector-ref a 1) (vector-ref b 1)))
  (define dy (- (vector-ref a 2) (vector-ref b 2)))

  (sqrt (+ (expt dx 2) (expt dy 2)))
  )

(define (city-locations count scale)
  (for/list ([i count])
    (vector i (* (random) scale) (* (random) scale))))
