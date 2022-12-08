#lang racket
(define data (port->string (open-input-file "./aoc2.txt")))

(define (spread funlist . operandlist)
  (map (lambda (f)
         (apply f operandlist)) funlist))
(spread (list + -) 1 2 3)

(define mine
  (map (compose (curry map char->integer) string->list)
   (string-split data #rx"\n")))
;rock, paper, scissors
;A,    B,     C
;X,    Y,     Z
;Wins: 1, -2
;Losses: 2, -1
;2 is a loss
;1 is a win
;0 is a draw

mine

(let ((X (char->integer #\X))
      (A (char->integer #\A)))
  (foldl
    (lambda (l acc)
      (+ acc
         (- (caddr l) X -1)
         (* 3 (modulo (- (caddr l) (car l) (- X A) -1) 3))))
    0
    (map (compose (curry map char->integer) string->list)
     (string-split data #rx"\n"))))

(map
    (lambda (l)
        (let ((X (char->integer #\X))
              (A (char->integer #\A)))
             (* 3 (modulo (- (caddr l) (car l) (- X A) -1) 3))))
    mine)

(apply +
 (spread
   (list
     (compose (curry apply +)
      (curry map
             (lambda (l)
               (* 3 (modulo (+ 1  (- (caddr l) (car l) 23)) 3)))))
     (compose (curry apply +)
      (curry map
             (lambda (l)
               (- (caddr l) (char->integer #\X) -1)))))
  mine))

;part 2, 
;Loss, Draw, Win
;X     Y     Z
;Win = 1 more
;Draw = 2 more
;Loss = 3 more
;Mod 3
(apply +
 (map
   (lambda (l)
     (+ 1
      (modulo
       (+
        (- (car l) (char->integer #\A))
        (- (caddr l) (char->integer #\X) 1))
       3)
      (* 3 (- (caddr l) (char->integer #\X)))))
   mine))
