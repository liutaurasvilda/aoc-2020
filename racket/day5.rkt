#lang racket

(define input (map (λ (e) (string->list e)) (file->lines "inputs/day5.txt")))

(define (locate rows start end)
  (cond [(null? rows) start]
        [(or (equal? (car rows) #\F) (equal? (car rows) #\L))
         (locate (cdr rows) start (truncate (/ (+ start end) 2)))]
        [(or (equal? (car rows) #\B) (equal? (car rows) #\R))
         (locate (cdr rows) (add1 (truncate (/ (+ start end) 2))) end)]))

(define (seat rows groups)
  (+ (* (locate rows 0 127) 8)
     (locate groups 0 7)))

(define (missing-seat input)
  (cond [(null? input) input]
        [(not (= (add1 (car input)) (cadr input))) (add1 (car input))]
        [(missing-seat (cdr input))]))

; part 1
(apply max (map (λ (e) (seat (take e 7) (drop e 7))) input))

; part 2
(missing-seat (sort (map (λ (e) (seat (take e 7) (drop e 7))) input) <))