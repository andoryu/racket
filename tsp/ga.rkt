#lang racket/gui
(require list-util)

(require "common.rkt")

; main calling point
; creates the default population, runs it for the generations and then returns
; the best of the final generation
(define (ga-path cities pop-size generation-limit)
  (define pop (create-population pop-size (length cities)))

  (do-genetic-algorithm pop generation-limit cities)
  '())

;utility function to create a population of agents based on the two parameters
(define (create-population number city-count)
  (build-list number (lambda (x)
                       (flatten
                         (cons 0
                            (cons (shuffle (range 1 city-count)) 0) )
                                   ))))


(define (do-genetic-algorithm pop generation-limit cities)
  (define (generation-cycle current limit pop)
    )
  )


  
(provide (all-defined-out))