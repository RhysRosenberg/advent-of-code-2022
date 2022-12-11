#lang racket
; pos are all pairs
(define (position-distance pos-1 pos-2)
  (list
    (- (car pos-2) (car pos-1))
    (- (cadr pos-2) (cadr pos-1))))

(define (sign num)
  (cond
    [(= num 0) 0]
    [(> num 0) 1]
    [true -1]))

(define (far-away? pos-dist (threshold 2))
  (ormap (compose (curry <= threshold) abs) pos-dist))

(define (move-towards tail-pos head-pos)
  (let ((dist (position-distance tail-pos head-pos)))
    (if (far-away? dist) ;if far away, move towards by one step, otherwise stay still
        (map +
         (map sign dist)
         tail-pos)
        tail-pos)))

(define (pos-add pos1 pos2)
  (list (+ (car pos1) (car pos2))
        (+ (cadr pos1) (cadr pos2))))

(define (repeat n val)
  (if (= n 0) '()
    (cons val (repeat (- n 1) val))))
; we need a function that applies a dyad onto an initializer and the car, then put that as the first car
; the next car will be that last placed car, and the next elem of the list

(define (accumulate-w-init op initializer lst)
  (if (null? lst) null
    (let ((x (op (car lst) initializer)))
      (cons x (accumulate-w-init op x (cdr lst))))))

; commands passed as string of "[Direction] [Amount]"
(define (process-command commands tail-amt (head-pos '(0 0)) (tails (repeat tail-amt '(0 0)))
                 (visited (set '(0 0))) [direction null] [distance 0])
  (cond
    [(and (null? commands) (= 0 distance)) (set-count visited)]
    [(= 0 distance)
     (let* ((command (string-split (car commands) " "))
            (new-direction (car command))
            (new-distance (string->number (cadr command))))
       (process-command (cdr commands) tail-amt head-pos tails visited new-direction new-distance))]
    [true
      (let* ((new-head-pos (pos-add head-pos
                                    (match direction
                                      ["U" '(0 1)]
                                      ["D" '(0 -1)]
                                      ["R" '(1 0)]
                                      ["L" '(-1 0)])))
             (new-tails (accumulate-w-init move-towards new-head-pos tails)))
        ;(printf "Head: ~a | Tail: ~a =>>  Head: ~a | Tail: ~a ~n" head-pos tails new-head-pos new-tails)
        (process-command commands tail-amt new-head-pos new-tails
                (set-add visited (last new-tails)) direction (- distance 1)))]))
(define test-input '("R 5" "U 8" "L 8" "D 3" "R 17" "D 10" "L 25" "U 20"))

(process-command test-input 9)
(process-command (file->lines "input.txt") 9)

