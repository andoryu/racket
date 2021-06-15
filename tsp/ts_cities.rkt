#lang racket/gui
(require racket/draw)
(require racket/include)

(require "nn.rkt")
(require "ga.rkt")


(define cities empty)
(define paths empty)

(define (city-locations count scale)
  (for/list ([i count])
    (vector i (* (random) scale) (* (random) scale))))

(define frame (new frame% [label "NN"] [width 1000] [height 1000]))

(define hpane (new horizontal-pane% [parent frame]))

(define bpanel (new vertical-panel% [parent hpane]
                                    [vert-margin 5]
                                    [horiz-margin 5]
                                    [stretchable-width 100]))

(define city-count (new text-field%
                        [label "Cities"]
                        [parent bpanel]
                        [init-value "10"]))


(define randomise-button (new button%
                           [parent bpanel]
                           [label "Randomise"]
                           [callback
                            (lambda (button event)
                              (set! cities (city-locations (string->number (send city-count get-value)) 1000))
                              (set! paths empty)
                              (send ts-canvas refresh)
                              )]
                           ))

(define nn-button (new button%
                       [parent bpanel]
                       [label "Nearest Neighbour"]
                       [callback
                        (lambda (button event)
                          (cond [(empty? cities) void]
                                [else
                                 (set! paths (nn-path cities))
                                 (send ts-canvas refresh)
                                 ]
                          ))]
                       ))

(define ga-button (new button%
                       [parent bpanel]
                       [label "GA"]
                       [callback
                        (lambda (button event)
                          (cond [(empty? cities) void]
                                [else
                                 (set! paths (ga-path cities 1000 500))
                                 (send ts-canvas refresh)
                                 ]
                          ))]
                       ))

(define ts-canvas (new canvas% [parent hpane]
     [min-width 1000]
     [stretchable-width 1000]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 1 1)
        (cond [(empty? paths) void]
              [else
               ;Lines
               (send dc set-brush "black" 'solid)
               (for ([path paths])
                 (send dc draw-line (vector-ref (car path) 1)
                             (vector-ref (car path) 2)
                             (vector-ref (cdr path) 1)
                             (vector-ref (cdr path) 2))
                 )
               ])
        (cond [(empty? cities) void]
              [else
               ;start city
               (send dc set-brush "green" 'solid)
               (send dc draw-ellipse (- (vector-ref (first cities) 1) 10)
                     (- (vector-ref (first cities) 2) 10)
                     20
                     20)
               ;remaining cities
               (send dc set-brush "blue" 'solid)
               (for ([city (rest cities)])
                 (send dc draw-ellipse (- (vector-ref city 1) 10)
                       (- (vector-ref city 2) 10)
                       20
                       20))               
              ]
        ))]))

(send frame show #t)
