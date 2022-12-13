#lang racket
(define test-input
  (map (curryr string-split "\n")
    (string-split
      (file->string "testinput.txt")
      "\n\n")))
(define input
  (map (curryr string-split "\n")
    (string-split
      (file->string "input.txt")
      "\n\n")))

(define (monkeys input)
   (map
     (lambda (lines)
       (list
         (cons 'id
               (string->number (second (regexp-match #px"Monkey (\\d+):" (list-ref lines 0)))))
         (cons 'items
               (map string->number
                 (string-split
                   (second (regexp-match #px"Starting items: ((?:\\d+(?:, )?)+)" (list-ref lines 1)))
                   ", ")))
         (cons 'op
               (cond
                 [(regexp-match #px"Operation: new = old \\+ (\\d+)" (list-ref lines 2))
                  => (lambda (mtch) (curry + (string->number (second mtch))))]
                 [(regexp-match #px"Operation: new = old \\* (\\d+)" (list-ref lines 2))
                  => (lambda (mtch) (curry * (string->number (second mtch))))]
                 [(regexp-match #px"Operation: new = old \\* old" (list-ref lines 2))
                  => (lambda (mtch) (curryr expt 2))]))
         (cons 'test
               (curryr
                 (lambda (n basis)
                   (integer? (/ n basis)))
                   ;(= 0 (modulo n basis)))
                 (string->number (second
                                  (regexp-match #px"divisible by (\\d+)" (list-ref lines 3))))))
         (cons 'dest
               (cons
                 (string->number (second (regexp-match #px"monkey (\\d+)" (list-ref lines 4))))
                 (string->number (second (regexp-match #px"monkey (\\d+)" (list-ref lines 5))))))
         (cons 'inspected-count 0)))
     input))

(define worry-reducer (lambda (worry) (floor (/ worry 3))))
; create an alist of (destination . worry-level) pairs that the monkey is tossing to
(define (monkey-toss monkey)
  (let ((items (cdr (assoc 'items monkey)))
        (op (cdr (assoc 'op monkey)))
        (test (cdr (assoc 'test monkey)))
        (dest (cdr (assoc 'dest monkey))))
    (map
      (lambda (item)
        (let ((new-worry (worry-reducer (op item))))
          (cons
            (if (test new-worry) (car dest) (cdr dest))
            new-worry)))
      items)))

; add an items to a monkey's item list
(define (add-to-monkey-items monkey item)
  (list-update
    monkey
    (index-of (map car monkey) 'items)
    (curryr append (list item))))

; perform one monkey's turn and return the updated monkey list
(define (monkey-turn monkeys index)
  (let ((monkey (list-ref monkeys index)))
    (list-update
      (foldl (lambda (dest-worry-pair monkeys)
               (list-update monkeys
                            (car dest-worry-pair)
                            (curryr add-to-monkey-items (cdr dest-worry-pair))))
             monkeys
             (monkey-toss monkey))
      ;(for/fold ([monkey-list monkeys])
                ;([dest-worry-pair (monkey-toss monkey)])
                ;(list-update monkey-list
                             ;(car dest-worry-pair)
                             ;(curryr add-to-monkey-items (cdr dest-worry-pair)))
      index
      (lambda (monkey)
        (list-set (list-update monkey
                               (index-of (map car monkey) 'inspected-count)
                               (lambda (inspec-pair)
                                 (cons 'inspected-count
                                       (+ (cdr inspec-pair)
                                          (length (cdr (assoc 'items monkey)))))))
                  (index-of (map car monkey) 'items)
                  '(items))))))

(define (do-round monkeys)
  (for/fold ([monkeys monkeys])
            ([monkey-index (in-range 0 (length monkeys))])
            (monkey-turn monkeys monkey-index)))

(define (after-n-rounds input n)
  (for/fold ([monkeys (monkeys input)])
            ([round-number (in-inclusive-range 1 n)])
            (do-round monkeys)))

(define (top-2-monkeys monkeys)
  (take (sort monkeys >
              #:key (lambda (monkey) (cdr (assoc 'inspected-count monkey))))
        2))
; test
"Part 1"

(apply *
  (map (compose cdr (curry assoc 'inspected-count))
    (top-2-monkeys
      (after-n-rounds test-input 20))))
;real
(apply *
  (map (compose cdr (curry assoc 'inspected-count))
    (top-2-monkeys
      (after-n-rounds input 20))))

;p2
"Part 2"

(set! worry-reducer identity)

;real
(apply *
    (map (compose cdr (curry assoc 'inspected-count))
         (top-2-monkeys (after-n-rounds input 200))))
