#lang racket

(define input (map (λ (e) (list->set (string->list e))) (file->lines "inputs/day6.txt")))

(define (join-group input group-answers answers)
  (cond [(null? input) (reverse (cons (length (set->list group-answers)) answers))]
        [(set-empty? (car input)) (join-group (cdr input) (set-clear group-answers) (cons (length (set->list group-answers)) answers))]
        [(join-group (cdr input) (set-union (car input) group-answers) answers)]))

(define (join-group-2 input group-answers answers)
  (cond [(null? input) (reverse (cons (length (set->list group-answers)) answers))]
        [(set-empty? (car input)) (join-group-2 (cdr input) (cadr input) (cons (length (set->list group-answers)) answers))]
        [(join-group-2 (cdr input) (set-intersect (car input) group-answers) answers)]))

; part1
(apply + (join-group input (set) '()))

; part2
(apply + (join-group-2 input (car input) '()))