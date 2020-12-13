#lang racket
(require rackunit)

(define input (file->lines "inputs/day13.txt"))

(define timestamp (string->number (car input)))
(define ids (string-split (cadr input) ","))

(struct bus (id t departs) #:transparent)

(define buses
  (map (λ (e) (bus (string->number e) 0 #t))
       (filter (λ (e) (not (equal? e "x"))) ids)))

(define (travel b)
  (let ([id (bus-id b)]
        [t (bus-t b)]
        [departs (bus-departs b)])
    (if (= (add1 t) id)
        (bus id 0 #t)
        (bus id (add1 t) #f))))

(check-equal? (bus 3 0 #t) (bus 3 0 #t))
(check-equal? (travel (bus 3 0 #t)) (bus 3 1 #f))
(check-equal? (travel (travel (bus 3 0 #t))) (bus 3 2 #f))
(check-equal? (travel (travel (travel (bus 3 0 #t)))) (bus 3 0 #t))

(define (depart timestamp ctime buses)
  (let ([curr-buses (filter (λ (e) (equal? (bus-departs e) #t)) buses)])
    (cond [(and (or (> ctime timestamp) (= ctime timestamp)) (not (null? curr-buses)))
           (* (- ctime timestamp) (bus-id (car curr-buses)))]
          [else (depart timestamp (add1 ctime) (map (λ (e) (travel e)) buses))])))

; part1
(depart timestamp 0 buses)