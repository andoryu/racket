#lang racket/gui
(new canvas% [parent hpane]
     [min-width 1000]
     [stretchable-width 1000]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 1 1)
        ;Lines
        (send dc set-brush "black" 'solid)
        (for ([path (zip nn-list (rest nn-list))])
          (send dc draw-line (vector-ref (car path) 1)
                             (vector-ref (car path) 2)
                             (vector-ref (cdr path) 1)
                             (vector-ref (cdr path) 2))
          )

        ;start city
        (send dc set-brush "green" 'solid)
          (send dc draw-ellipse (- (vector-ref (first nn-list) 1) 10)
                                (- (vector-ref (first nn-list) 2) 10)
                                20
                                20)
        
        ;remaining cities, minus the return to start city
        (send dc set-brush "blue" 'solid)
        (for ([city (drop-right (rest nn-list) 1)])
          (send dc draw-ellipse (- (vector-ref city 1) 10)
                                (- (vector-ref city 2) 10)
                                20
                                20))
        )])




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



(if (and (>= x u)
         (<= x v))
    1 2)