#lang racket
(define input (file->lines "input.txt"))
(define test-input (file->lines "testinput.txt"))

(define (beam-intersection sprite-location current-pixel)
  (>= 1 (abs (- (modulo current-pixel 40) sprite-location))))

(define (step instructions cycle-count X [executing? #f] [update #f])
  ;(sleep 1)
  (let ((X (if update (+ X update) X)))
    ;(printf "X:~a tgt:~a ~n" X (if (beam-intersection X cycle-count) #\# #\.))
    (if (null? instructions) null
      (cons (if (beam-intersection X cycle-count) #\# #\.)
            (cond
                [(regexp-match #px"addx (-?\\d+)" (car instructions))
                 =>
                 (lambda (mtch)
                   (if executing?
                     (step (cdr instructions)
                           (add1 cycle-count)
                           X
                           #f
                           (string->number (cadr mtch)))
                     (step instructions
                           (add1 cycle-count)
                           X
                           #t)))]
                [(regexp-match #px"noop" (car instructions))
                 (step (cdr instructions)
                       (add1 cycle-count)
                       X)])))))

(define (beam input)
  (step input 0 1))

(define (render-screen beam)
  (if (null? beam) null
    (begin
      (printf "~a~n" (list->string (take beam 40)))))
  (render-screen (drop beam 40)))
(render-screen (beam input))

