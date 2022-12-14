(define filename "input.txt")
(define input-strings
  (map (lambda (s) (string-split s "\n"))
    (string-split
     (file->string filename) 
     "\n\n")))


(require peg)
(define-peg start-list
            (char #\[))
(define-peg end-list
            (char #\]))
(define-peg number (name res (+ (range #\0 #\9)))
            (string->number res))
(define-peg list-expr
            (and start-list
                 (name contents
                       (and
                         (? (or list-expr number))
                         (* (drop (char #\,))
                            (or list-expr number))))
                 end-list)
            contents)

(define (parse-list str)
  (peg list-expr str))

(define input-lists
  (map (lambda (is)
         (map parse-list is))
    input-strings))

; returns 'ordered 'unordered or #f if inconclusive
(define (compare-lists l1 l2)
  (cond
    [(andmap null? (list l1 l2)) #f]
    [(null? l1) 'ordered]
    [(null? l2) 'unordered]
    [(andmap (compose number? car) (list l1 l2))
     (cond
       [(< (car l1) (car l2)) 'ordered]
       [(> (car l1) (car l2)) 'unordered]
       [true (compare-lists (cdr l1) (cdr l2))])]
    [true
     (let ((result (apply compare-lists
                          (map
                            (compose
                              (lambda (e)
                                (if (list? e) e (list e)))
                              car)
                            (list l1 l2)))))
       (if result result (compare-lists (cdr l1) (cdr l2))))]))


(define ordering
  (map
    (curry apply compare-lists)
    input-lists))

(for/fold
  ([sum 0])
  ([(ordered? i) (in-indexed ordering)])
  (if (eq? ordered? 'ordered) (+ sum (add1 i)) sum))

; part 2
(let ((sorted-input-lists
        (sort
          (append
            (apply append input-lists)
            '(((2)))
            '(((6))))
          (lambda (l1 l2)
            (eq?
              'ordered
              (compare-lists l1 l2))))))
  (foldl * 1
    (map (lambda (e)
           (add1
             (index-of sorted-input-lists e)))
         (list '((2)) '((6))))))


