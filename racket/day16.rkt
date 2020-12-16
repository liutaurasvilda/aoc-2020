#lang racket

(define input
  (map (λ (pair) (list (string->number (car (string-split pair "-"))) (string->number (cadr (string-split pair "-")))))
       (string-split (car (file->lines "inputs/test.txt")) ",")))

(define nearby (map (λ (e) (string->number e)) (string-split (car (file->lines "inputs/nearby.txt")) ",")))

(define (build-range start end)
  (sequence->list (in-range start (add1 end))))

(define valid-ranges (flatten (map (λ (e) (build-range (car e) (cadr e))) input)))

(define (valid ticket range)
  (not (equal? (member ticket range) #f)))

(define (count-invalid tickets valid-range acc)
  (cond [(null? tickets) acc]
        [(valid (car tickets) valid-range) (count-invalid (cdr tickets) valid-range acc)]
        [else (count-invalid (cdr tickets) valid-range (+ acc (car tickets)))]))

; part1
(count-invalid nearby valid-ranges 0) 