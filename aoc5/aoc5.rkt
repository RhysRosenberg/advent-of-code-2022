(define data
 (let ((filename "./aoc5.txt"))
    (string-split (port->string (open-input-file filename))
                  #rx"\n\n")))

(define (push stacks k elem)
  (let ((s (hash-ref stacks k)))
    (hash-set! stacks k (cons elem s))))

(define structure
 (let* ((inp (reverse (string-split (car data) #rx"\n")))
        (stacks (make-hash))
        (nums (map string->number
                (regexp-match* #rx"[0-9]+" (car inp)))))
   (map (lambda (i) (hash-set! stacks i '())) nums)
   (map (lambda (row)
          (for ([(mtch i) (in-indexed (regexp-match* #px" {4}|\\[[A-Z]\\]" row))])
            (when (not (equal? "    " mtch))
              (push stacks (+ 1 i) mtch))))
     (cdr inp))
   stacks))

(define commands
  (let ((inp (cadr data)))
       (map
         (compose
           (curry map string->number)
           (curry regexp-match* #rx"[0-9]+"))
         (string-split inp #rx"\n"))))
(print commands)


(define (move stacks k1 k2)
  (let ((s1 (hash-ref stacks k1))
        (s2 (hash-ref stacks k2)))
    (hash-set! stacks k1 (cdr s1))
    (hash-set! stacks k2 (cons (car s1) s2))))

(define (repeater thunk count) (for ((i (in-range count))) (thunk)))

(for ([cmd commands])
  (repeater (lambda () (move structure (cadr cmd) (caddr cmd))) (car cmd)))

(for ((stack (in-hash-values structure)))
  (print (car stack)))

(define (move-9001 stacks k1 k2 amt)
  (let ((s1 (hash-ref stacks k1))
        (s2 (hash-ref stacks k2)))
    (hash-set! stacks k1 (drop s1 amt))
    (hash-set! stacks k2 (append (take s1 amt) s2))))

(for ([cmd commands])
  (move-9001 structure (cadr cmd) (caddr cmd) (car cmd)))
(print structure)
