(define data
 (let ((filename "./aoc3.txt"))
  (map
    (compose
       string->list
       string-trim)
    (string-split (port->string (open-input-file filename))
                  #rx"\n"))))
(define splitted
  (map
    (lambda (l)
      (let ((len (/ (length l) 2)))
        (list
          (take l len)
          (drop l len))))
    data))

(define doubles
  (flatten
    (map
      (curry apply set-intersect)
      splitted)))

(define (prioritize vals)
 (apply +
   (let ((a (char->integer #\a))
         (A (char->integer #\A)))
     (map (lambda (c)
            (let ((i (char->integer c)))
              (+ 1 (if (>= i a) (- i a) (+ 26 (- i A))))))
          vals))))
(prioritize doubles)

; Part 2

(define (groupsof n l)
  (if (null? l)
    l
    (let ((l1 (take l n)))
      (cons l1 (groupsof n (drop l n))))))

(define grouped (groupsof 3 data))
(prioritize
  (flatten
    (map
     (curry apply set-intersect)
     grouped)))
