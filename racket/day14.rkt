#lang racket
(require srfi/13)
(require rackunit)

(define input (file->lines "inputs/day14.txt"))

(define (dec->bin n)
  (let ([b (letrec ((recc (lambda (acc n)
                            (cond ((zero? n) acc)
                                  (else (recc (cons (remainder n 2) acc)
                                              (quotient n 2)))))))
             (recc '() n))])
    (append (make-list (- 36 (length b)) 0) b)))

(check-equal? (dec->bin 11) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1))
(check-equal? (dec->bin 73) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 1))

(define (bin->dec n)
  (if (zero? n)
      n
      (+ (modulo n 10) (* 2 (bin->dec (quotient n 10))))))

(check-equal? (bin->dec 000000000000000000000000000000001011) 11)
(check-equal? (bin->dec 000000000000000000000000000001001001) 73)

(define (b->n l)
  (string->number (string-join (map (λ (e) (number->string e)) l) "")))

(check-equal? (b->n '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1)) 1011)

(define (mask-mapper m b acc)
  (cond [(null? m) acc]
        [(equal? (car m) "X") (mask-mapper (cdr m) (cdr b) (append acc (list (car b))))]
        [else (mask-mapper (cdr m) (cdr b) (append acc (list (string->number (car m)))))]))

(define (mask-map mask value)
  (let* ([m (map string (string->list mask))]
        [b (dec->bin value)]
        [mapped (mask-mapper m b '())])
    (bin->dec (b->n mapped))))

(define (bitmask input memory mask)
  (cond [(null? input) (apply + (hash-values memory))]
        [(string-contains? (car input) "mask") (bitmask (cdr input) memory (substring (car input) 7))]
        [else
         (let ([loc (substring (car input) 4 (string-contains (car input) "]"))]
               [val (string->number (substring (car input) (+ (string-contains (car input) "=") 2)))])
           (bitmask (cdr input) (hash-set memory loc (mask-map mask val)) mask))]))

; part1
(bitmask input (make-immutable-hash) "")