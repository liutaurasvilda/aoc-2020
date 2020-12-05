#lang racket

(define input (map (λ (e) (string->list e)) (file->lines "inputs/day5.txt")))

(define (find-seat rows start end)
  (cond [(null? rows) start]
        [(or (equal? (car rows) #\F) (equal? (car rows) #\L))
         (find-seat (cdr rows) start (truncate (/ (+ start end) 2)))]
        [(or (equal? (car rows) #\B) (equal? (car rows) #\R))
         (find-seat (cdr rows) (add1 (truncate (/ (+ start end) 2))) end)]))

(define (seat rows groups)
  (+ (* (find-seat rows 0 127) 8)
     (find-seat groups 0 7)))

(define (find-missing-seat input)
  (cond [(null? input) input]
        [(not (= (add1 (car input)) (cadr input))) (add1 (car input))]
        [(find-missing-seat (cdr input))]))

; part 1
(apply max (map (λ (e) (seat (take e 7) (drop e 7))) input))

; part 2
(find-missing-seat (sort (map (λ (e) (seat (take e 7) (drop e 7))) input) <))