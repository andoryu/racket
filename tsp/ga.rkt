#lang racket/gui
(require list-util)

(require "common.rkt")

(provide (all-defined-out))

;entry point
(define (ga-path cities pop-size generation-limit)
  (define pop (create-population pop-size (length cities)))

  (define final-pop (generation pop generation-limit cities))
  ;(printf "~a\n" (list-ref final-pop 0))

  (define city-list (indexes->cities (list-ref final-pop 0) cities))
  ;(printf "~a\n" city-list)

  (zip city-list (rest city-list))
  )

; translate the list of indexes to a full city list for the plotter
(define (indexes->cities agent cities)
  (for/list ([city agent])
    (list-ref cities city)
    )
  )

;utility function to create a population of agents based on the two parameters
(define (create-population number city-count)
  (build-list number (lambda (x)
                       (flatten
                         (cons 0
                            (cons (shuffle (range 1 city-count)) 0) )
                                   ))))

; process generation through limit cycles to find the best agents
(define (generation pop limit cities)
  (define agents pop)
  (define pop-size (length pop))
  (for ([gen limit])
    (set! agents (create-new-generation (measure-fitness agents cities) pop-size))
    ;(printf "gen: ~a, len:~a\n" gen (length (list-ref agents 0)))
    )
  agents
  )

(define (create-new-generation pop-fitness pop-total)
  (define keep (quotient (length pop-fitness) 10))
  (define parents (take (sort pop-fitness #:key cdr <) keep))
  (define wheel (roulette-wheel parents))

  (define (random-parent)
    (car (list-ref parents (random keep))))
  
  (build-list pop-total
              (lambda (x)
                (do-crossover (random-parent) (random-parent))
                )
              )
 )

(define (do-crossover a b)
  (define u (random (length a)))
  (define v (random (length a)))

  (cond
    [(= u v) (single-crossover a b u)]
    [(> u v) (dual-crossover a b v u)]
    [(< u v) (dual-crossover a b u v)])
  )

(define (single-crossover a b u)
  (cond
    [(zero? u) b]
    [(= u (sub1 (length a))) a]
    [else
     (define a-part (take a u))
     (define b-rest (remove* a-part b))
     (append a-part b-rest '(0))
     ]
    )
  )

(define (dual-crossover a b u v)
  (define a-part1 (if (zero? u) '(0) (take a u)))
  (define a-part2 (list-tail a v))
  (define b-rest (remove* (append a-part1 a-part2) b))

  (append a-part1 b-rest a-part2)
  )

;create the roulette wheel for this generation that
;has a number of entries proportional to the fitness of the critter
(define (roulette-wheel peak-pop)
  (define weights (roulette-weights peak-pop))
  (flatten
   (for/list ([weight weights])
    (build-list (exact-round (cdr weight)) (lambda (x) (car weight)))))
  )

(define (roulette-weights peak-pop)
  (define max-path-dist (cdr(argmax cdr peak-pop)))

  (for/list ([i peak-pop] [index (length peak-pop)])
    (cons index
          (+ (- max-path-dist (cdr i)) 1))
    )  
  )

(define (measure-fitness pop cities)
  (build-list (length pop)
              (lambda (x)
                (cons (list-ref pop x) (calc-path (list-ref pop x) cities))
               )
   )
 )

(define (calc-path agent cities)
   (foldl (lambda (x result)
            (let
              ([a (list-ref cities (car x))]
               [b (list-ref cities (cdr x))])
            (+ result (calc-distance a b))))
          0
          (zip agent (rest agent))
   )
 )
