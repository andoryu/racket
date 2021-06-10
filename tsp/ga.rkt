#lang racket/gui
(require list-util)

(require "common.rkt")

(define pop empty)

(define (create-population number city-count)
  (build-list number (lambda (x)
                       (flatten
                         (cons 0
                            (cons (shuffle (range 1 city-count)) 0) )
                                   ))))

(define (calc-distances pop cities)
  (for/list ([p pop])
    (cons p (foldl (lambda (a result)
             (+ result
                (calc-distance (list-ref cities (car a)) (list-ref cities (cdr a)))))
           0 
           (zip p (rest p))))))



;create the roulette wheel for this generation that
;has a number of entries proportional to the fitness of the critter
(define (roulette-weights peak-pop)
  (define max-path-dist (cdr(argmax cdr peak-pop)))

  (for/list ([i peak-pop] [index (length peak-pop)])
    (cons index
          (+ (- max-path-dist (cdr i)) 1))
    )  
  )

(define (roulette-wheel peak-pop)
  (define weights (roulette-weights peak-pop))
  (flatten
   (for/list ([weight weights])
    (build-list (exact-round (cdr weight)) (lambda (x) (car weight)))))
  )


(define (perform-cross-over a b u v)
  (define mask
    (build-list (length a)
                (lambda (x)
                  (and (>= x u)
                       (<= x v)))))
  
  (set! mask (cons #t (rest mask)))
  (set! mask (flatten (cons (list-tail mask 1) #t)))
  
  mask)

; perform cross-over between two parents
(define (cross-over a b)
  (define u (+ (random (- (length a) 2))))
  (define v (+ (random (- (length b) 2))))

  (cond
    [(equal? u v)
     a]
    [(< v u)
     (perform-cross-over a b v u)]
    [else
     (perform-cross-over a b u v)])
 )


; Keep the best 10 percent and proportionally reproduce based on performance
(define (generation-cycle sorted-pop)
  (define len (quotient (length sorted-pop) 10))
  (define parents (take sorted-pop len))
  (define wheel (roulette-wheel parents))
  (define wheel-size (length wheel))

  (println wheel-size)
  
  (for/list ([i (range (length sorted-pop))])
    (car (list-ref parents
                   (list-ref wheel (random wheel-size)))))
  )

; Sort agents & total distance in ascending order
(define (sort-asc pop-distances)
  (sort pop-distances
        (lambda (x y)
          (< (cdr x) (cdr y)))))

(define (genetic-algorithm generation-limit cities)
  (for ([g (range generation-limit)])
    (printf "generation ~a\n" g)
    (generation-cycle
     (sort-asc (calc-distances pop cities)))
  ))

;create a full city object list from the index list
(define (build-full-path path-list cities)
  (for/list ([p path-list])
    (list-ref cities p)))

; main calling point
; creates the default population, runs it for the generations and then returns
; the best of the final generation
(define (ga-path cities pop-size generation-limit)
  (set! pop (create-population pop-size (length cities)))

  (genetic-algorithm generation-limit cities)

  (println pop)
  
  (let ([city-order (build-full-path
                     pop
                     cities)])
        (zip city-order (rest city-order))
        ))

(provide (all-defined-out))

;============================
;============================
;=== Dev tests ===

;(define cities empty)
;(define (city-locations count scale)
;  (for/list ([i count])
;    (vector i (* (random) scale) (* (random) scale))))
;(set! cities (city-locations 20 1000))
;(ga-path cities 20 5)


;
;(set! pop (create-population 10 20))
;
;(calc-distances pop cities)
;(genetic-algorithm 2 cities)