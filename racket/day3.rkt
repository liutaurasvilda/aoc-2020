#lang racket

(define input
  (map (λ (e) (string->list e))
       (file->lines "inputs/day3.txt")))

(define (count-trees-h input slope-r slope-d position-r position-d trees)
  (if (null? input) trees
      (cond [(= position-d slope-d)
             (if (equal? (list-ref (car input) (modulo (+ position-r slope-r) (length (car input)))) #\#)
                 (count-trees-h (cdr input) slope-r slope-d (modulo (+ position-r slope-r) (length (car input))) 1 (+ trees 1))
                 (count-trees-h (cdr input) slope-r slope-d (modulo (+ position-r slope-r) (length (car input))) 1 trees))]
            [else (count-trees-h (cdr input) slope-r slope-d position-r (+ position-d 1) trees)])))

(define (count-trees input slope-right slope-down)
  (count-trees-h input slope-right slope-down 0 1 0))
  
; part 1
(define (count-trees-1 input)
  (count-trees (cdr input) 3 1))

; part 2
(define (count-trees-2 input)
  (*
   (count-trees (cdr input) 1 1)
   (count-trees (cdr input) 3 1)
   (count-trees (cdr input) 5 1)
   (count-trees (cdr input) 7 1)
   (count-trees (cdr input) 1 2)
   ))