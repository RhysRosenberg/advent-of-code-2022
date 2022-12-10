
(define test-forest
  (map (compose (curry map (lambda (c) (- (char->integer c) (char->integer #\0)))) string->list)
       (string-split
         "30373\n25512\n65332\n33549\n35390" "\n")))

(define forest
  (map
    (compose
      (curry map (lambda (c) (- (char->integer c) (char->integer #\0)))) 
      string->list)
    (file->lines "input.txt")))

(define (from-above lst)
  (if
    (null? (car lst)) null
    (cons (map car lst) (from-above (map cdr lst)))))
(print forest)
(from-above forest)

(define (split-around lst i)
  (let-values (((a b) (split-at lst i)))
    (values a (car b) (cdr b))))

(define (look-across lst num)
  (cond
    [(null? lst) 0]
    [(>= (car lst) num) 1]
    [true (+ 1 (look-across (cdr lst) num))]))

(define (horizontal-scores forest)
  (map (lambda (line)
         (for/list ([(t i) (in-indexed line)])
           (let-values (((a mid b) (split-around line i)))
             (* (look-across (reverse a) mid)
                (look-across b mid)))))
    forest))
(define (vertical-scores forest)
  (from-above (horizontal-scores (from-above forest))))

(apply max
  (flatten
    (map
      (lambda (f1 f2)
        (map * f1 f2))
      (horizontal-scores forest)
      (vertical-scores forest))))




