#lang racket
(require algorithms)

(define filename "input.txt")

(define entry-point '(500 0))

(define wall-coordinates
  (map
    (compose
        (curry map
               (compose
                 (curry map string->number)
                 (curryr string-split ",")))
        (curryr string-split " -> "))
    (file->lines filename)))

(define wall-coord-pairs
  (map
    (curry adjacent-map list)
    wall-coordinates))

; generate all coordinates between two points
(define (all-coords-between c1 c2)
  (let ((diff (map (compose (lambda (x) (if (= x 0) 0 (if (< x 0) -1 1))) -) c2 c1)))
    (if (andmap = c1 c2)
      (list c1)
      (let ((ci (map + c1 diff)))
        (cons c1 (all-coords-between ci c2))))))

(define all-coords (for*/list ([pair-list wall-coord-pairs]
                               [pair pair-list]
                               [coord (apply all-coords-between pair)])
                       coord))


(define max-Y
  (foldl (lambda (a acc) (max (second a) acc)) 0 all-coords))

; fall test should return #f if it could not fall, #t if it did,
; and 'end if it fell all the way

(define (get-cave-slice cave source [max-Y max-Y])
  (for/list ((y (in-inclusive-range (second source) max-Y)))
    (hash-ref cave (list (first source) y) #f)))

(define (unobstructed? cave current-coord X-offset)
  (not (hash-ref cave (list (+ (first current-coord) X-offset)
                            (add1 (second current-coord)))
                 #f)))

(define (fall cave source)
  (let* ([slice (get-cave-slice cave source)]
         [next-obstacle (index-where slice identity)])
    (if next-obstacle
      (let ([fell-to (list (first source) (+ (second source) (sub1 next-obstacle)))])
        (cond
          [(unobstructed? cave fell-to -1) (fall cave (map + fell-to '(-1 1)))]
          [(unobstructed? cave fell-to +1) (fall cave (map + fell-to '(+1 1)))]
          [else fell-to]))
      #f)))

(define cave
  (let ((new-cave (make-hash)))
    (for-each (lambda (coord) (hash-set! new-cave coord 'rock)) all-coords)
    new-cave))

(do
 ([fell-to (fall cave entry-point) (fall cave entry-point)])
 ((not fell-to) cave)
 (hash-set! cave fell-to 'sand))

(length
  (filter
    (lambda (val) (eq? val 'sand))
    (hash-values cave)))

; p2


(define p2-cave
  (let ((new-cave (make-hash)))
    (for-each (lambda (coord) (hash-set! new-cave coord 'rock))
              all-coords)
    (for-each (lambda (coord) (hash-set! new-cave coord 'rock))
              (all-coords-between (list 0 (+ 2 max-Y)) (list 1000 (+ 2 max-Y))))
    new-cave))


(define (p2-fall cave source max-Y)
  (let* ([slice (get-cave-slice cave source (+ 2 max-Y))]
         [next-obstacle (index-where slice identity)])
    (if next-obstacle
      (let ([fell-to (list (first source) (+ (second source) (sub1 next-obstacle)))])
        (cond
          [(unobstructed? cave fell-to -1) (p2-fall cave (map + fell-to '(-1 1)) max-Y)]
          [(unobstructed? cave fell-to +1) (p2-fall cave (map + fell-to '(+1 1)) max-Y)]
          [else fell-to]))
      #f)))

(do
 ([fell-to (p2-fall p2-cave entry-point max-Y) (p2-fall p2-cave entry-point max-Y)])
 ((andmap = fell-to '(500 0)) p2-cave)
 (hash-set! p2-cave fell-to 'sand))

(add1
  (length
    (filter
      (lambda (val) (eq? val 'sand))
      (hash-values p2-cave))))


