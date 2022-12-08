(define test-commands
  (file->lines "aoc7test.txt"))
(define commands
  (file->lines "aoc7.txt"))

(define (process commands current-directory files directories)
  (if (null? commands) 
    (values files directories)
    (cond
      [(regexp-match #px"cd (\\w+)" (car commands)) 
       => (lambda (mtch)
            (let ((new-dir
                    (string-append
                      current-directory
                      (if (equal? current-directory "/") "" "/")
                      (cadr mtch))))
              (process
                (cdr commands)
                new-dir
                files
                (set-add directories new-dir))))]
      [(regexp-match #px"cd .." (car commands)) 
       (process
        (cdr commands)
        (regexp-replace #px"/[^/]+$" current-directory "")
        files
        directories)]
      [(regexp-match #px"(\\d+) (.+$)" (car commands))
       => (lambda (mtch)
            (let ((file-name (string-append current-directory "--" (caddr mtch)))
                  (file-size (string->number (cadr mtch))))
              (process
                (cdr commands)
                current-directory
                (hash-set files file-name file-size)
                directories)))]
      [true (process (cdr commands) current-directory files directories)])))

(let-values (((files directories) (process test-commands "/" (make-immutable-hash) (set "/"))))
  (apply +
    (filter
      (curry >= 100000)
      (map
          (lambda (dir)
            (apply +
               (map cdr
                 (filter
                   (lambda (file) (string-prefix? (car file) dir))
                   (hash->list files)))))
          (set->list directories)))))

(let-values (((files directories) (process commands "/" (make-immutable-hash) (set "/"))))
  (apply +
    (filter
      (curry >= 100000)
      (map
          (lambda (dir)
            (apply +
               (map cdr
                 (filter
                   (lambda (file) (string-prefix? (car file) dir))
                   (hash->list files)))))
          (set->list directories)))))


(define all-dirs
  (let-values (((files directories) (process commands "/" (make-immutable-hash) (set "/"))))
        (map
            (lambda (dir)
              (cons dir
                (apply +
                   (map cdr
                     (filter
                       (lambda (file) (string-prefix? (car file) dir))
                       (hash->list files))))))
            (set->list directories))))

all-dirs
(let* ((current-size (cdr (assoc "/" all-dirs)))
       (needed-size (- 30000000 (- 70000000 current-size))))
  (apply min
    (filter (curry <= needed-size) (map cdr all-dirs))))
