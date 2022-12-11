#lang racket
(define testinput (file->lines "testinput.txt"))
(define input (file->lines "input.txt"))
(define (step instructions cycle-count X [executing? #f] [update #f])
  (let ((X (if update update X)))
    (cond
      [(null? instructions) null]
      [(regexp-match #px"addx (-?\\d+)" (car instructions))
       =>
       (lambda (mtch)
         ;(sleep 1)
         ;(printf "cyc : ~a | X : ~a ~n" cycle-count X)
         (if executing?
           (let ((new-X (+ X (string->number (cadr mtch)))))
             (cons new-X
                   (step (cdr instructions) (add1 cycle-count) X #f new-X)))
           (cons X
                 (step instructions (add1 cycle-count) X #t))))]
      ["noop"
       (cons X
             (step (cdr instructions) (add1 cycle-count) X))]
      [true
        (step (cdr instructions) cycle-count X)])))

(define history
  (step input 1 1))

(define (signal-strength history time)
  (list-ref history (- time 2)))
(signal-strength history 220)

(foldl + 0
  (map
    *
    '(20 60 100 140 180 220)
    (map
      (lambda (t) (signal-strength history t))
      '(20 60 100 140 180 220))))
