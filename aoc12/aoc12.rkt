(define filename "input.txt")
(define (ascii-to-num c)
  (-
    (char->integer c)
    (char->integer #\a)))

(define (pair-do op c1 c2)
  (cons (op (car c1) (car c2))
        (op (cdr c1) (cdr c2))))
(define (pair-either c)
  (or (car c) (cdr c)))
(define (pair-both c)
  (and (car c) (cdr c)))

(define (index-map my-map c)
  (list-ref (list-ref my-map (cdr c)) (car c)))

(define (get-neighbors coord map-size)
  (let ((neighbor-offsets '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
        (coord-positioner (curry pair-do + coord)))
    (filter
      (lambda (c)
        (and
          (pair-both
            (pair-do < c map-size))
          (pair-both
            (pair-do >= c '(0 . 0)))))
      (map coord-positioner neighbor-offsets))))

(define (map-with-original func lst)
  (map
    (lambda (elem)
      (cons elem (func elem)))
    lst))


;(unweighted-graph/directed)

(define mountain
  (map
    (compose
      (curry map (lambda (c) (cond
                               [(char=? c #\S) 0]
                               [(char=? c #\E) 25]
                               [true (ascii-to-num c)])))
      string->list)
    (file->lines filename)))

(define start-position
  (let* ((text-mountain (file->lines filename))
         (j (index-where text-mountain (lambda (s) (string-contains? s "S"))))
         (i (index-of (string->list (list-ref text-mountain j)) #\S)))
    (cons i j)))

(define end-position
  (let* ((text-mountain (file->lines filename))
         (j (index-where text-mountain (lambda (s) (string-contains? s "E"))))
         (i (index-of (string->list (list-ref text-mountain j)) #\E)))
    (cons i j)))

(define mountain-size
  (cons (length (car mountain))
        (length mountain)))

(define all-indices
  (map
    (lambda (p)
      (cons
        (car p)
        (cadr p)))
    (cartesian-product (stream->list (in-range 0 (car mountain-size))) 
                       (stream->list (in-range 0 (cdr mountain-size))))))

(define coords-and-moveable-neighbors
  (map (lambda (coord)
         (cons
           coord
           (filter
             (lambda (neighbor) (<= (index-map mountain neighbor)
                                    (+ 1 (index-map mountain coord))))
             (get-neighbors coord mountain-size))))
       all-indices))

(define coord-edge-lists
  (map
    (lambda (coord-and-moveable)
      (cartesian-product (list (car coord-and-moveable)) (cdr coord-and-moveable)))
    coords-and-moveable-neighbors))

(require graph)

(define mountain-graph
  (unweighted-graph/directed
    (foldl append '() coord-edge-lists)))

; p1
(sub1
  (length
    (fewest-vertices-path mountain-graph start-position end-position)))
;p2
(apply min
  (map
    (lambda (spos)
      (let ((path (fewest-vertices-path mountain-graph spos end-position)))
        (if path
          (sub1 (length path))
          99999)))
    (filter
           (lambda (index) ( = 0 (index-map mountain index)))
           all-indices)))

