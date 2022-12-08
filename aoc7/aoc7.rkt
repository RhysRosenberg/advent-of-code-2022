(define test-lines (file->lines "./aoc7test.txt"))
(define input-lines (file->lines "aoc7.txt"))

(define (execute commands current-dir files directories)
  (if (null? commands) (values files directories)
    (let ((cmd (car commands)))
      (cond
        [(regexp-match #px"cd (\\w+)" cmd)
         =>
         (lambda (mtch)
           (let ((new-dir (string-append current-dir "/" (cadr mtch))))
             (execute (cdr commands)
                      new-dir
                      files
                      (set-add directories new-dir))))]
        [(regexp-match #px"cd .." cmd)
         =>
         (lambda (mtch)
           (execute (cdr commands)
                    (regexp-replace #px"/[^/]*$" current-dir "")
                    files
                    directories))]
        [(regexp-match #px"(\\d+) (\\S+)" cmd)
         =>
         (lambda (mtch)
           (let ((filesize (cadr mtch)))
             (execute (cdr commands)
                      current-dir
                      (hash-set
                        files
                        (string-append current-dir "/" (caddr mtch))
                        (string->number filesize))
                      directories)))]
        [else (execute (cdr commands) current-dir files directories)]))))

(define (summarize files directories)
  (map
    (lambda (dir)
      (cons dir
        (foldl
         (lambda (pair acc) (+ acc (cdr pair)))
         0
         (filter
           (lambda (file)
             (string-prefix? (car file) dir))
           (hash->list files)))))
    (set->list directories)))

(let-values (((files directories)
              (execute test-lines "/" (make-immutable-hash) (set "/"))))
  (apply +
    (filter (curry >= 100000)
      (map
       cdr
       (summarize files directories)))))

(execute input-lines "/" (make-immutable-hash) (set "/"))

(let-values (((files directories)
              (execute input-lines "/" (make-immutable-hash) (set "/"))))
  (apply +
    (filter (curry >= 100000)
      (map
       cdr
       (summarize files directories)))))

