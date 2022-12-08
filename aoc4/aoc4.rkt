(require "intervals.rkt")
(define (interval-contains i1 i2)
  (and
    (>= (car i1) (car i2))
    (<= (cdr i1) (cdr i2))))
(define (interval-overlaps-anywhere i1 i2)
  (or
    (and
      (<= (car i2) (cdr i1)) ; If the start of i2 is before the end of i1
      (>= (car i2) (car i1))) ; and after the beginning of i1
    (and
      (>= (cdr i2) (car i1))
      (<= (cdr i2) (cdr i1)))))
(define (also combiner function arg1 arg2)
  (combiner
    (function arg1 arg2)
    (function arg2 arg1)))

(define data
 (let ((filename "./aoc4.txt"))
  (map
    (compose
      (curry map
             (compose
                 (lambda (l) (cons (car l) (cadr l)))
                 (curry map string->number)
                 (curryr string-split #rx"-")))
      (curryr string-split #rx",") ; (2-4 6-8)
      string-trim) ;2-4,6-8
    (string-split (port->string (open-input-file filename))
                  #rx"\n"))))
(print data)

; p1
(length
  (filter
    identity
    (map
      (lambda (ipair)
        (also (lambda (a b) (or a b)) interval-contains (car ipair) (cadr ipair)))
      data)))

; p2
(length
  (filter
    identity
    (map
      (lambda (ipair)
        (also (lambda (a b) (or a b)) interval-overlaps-anywhere (car ipair) (cadr ipair)))
      data)))
