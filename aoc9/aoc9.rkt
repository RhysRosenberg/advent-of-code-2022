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

; commands passed as string of "[Direction] [Amount]"
(define (process-command commands (head-pos '(0 0)) (tail-pos '(0 0))
                 (visited (set '(0 0))) [direction null] [distance 0])
  (cond
    [(and (null? commands) (= 0 distance)) (set-count visited)]
    [(= 0 distance)
     (let* ((command (string-split (car commands) " "))
            (new-direction (car command))
            (new-distance (string->number (cadr command))))
       (process-command (cdr commands) head-pos tail-pos visited new-direction new-distance))]
    [true
      (let* ((new-head-pos (pos-add head-pos
                                    (match direction
                                      ["U" '(0 1)]
                                      ["D" '(0 -1)]
                                      ["R" '(1 0)]
                                      ["L" '(-1 0)])))
             (new-tail-pos (move-towards tail-pos new-head-pos)))
        (printf "Head: ~a | Tail: ~a =>>  Head: ~a | Tail: ~a ~n" head-pos tail-pos new-head-pos new-tail-pos)
        (process-command commands new-head-pos new-tail-pos
                 (set-add visited new-tail-pos) direction (- distance 1)))]))
(define test-input '("R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2"))
(process-command test-input)
(process-command (file->lines "input.txt"))


