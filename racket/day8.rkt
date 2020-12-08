#lang racket

(define input (map (λ (e) (string-split e " ")) (file->lines "inputs/day8.txt")))

(define (sign s)
  (string->symbol (substring s 0 1)))

(define (value s)
  (string->number (substring s 1)))

(define (seen instructions i)
  (set-member? instructions i))

(define (instr input i)
  (car (list-ref input i)))

(define (instr2 input i)
  (cadr (list-ref input i)))
  
(define (calc input i acc exec)
  (cond [(seen exec i) acc]
        [(equal? (instr input i) "acc") (calc input (add1 i) ((eval (sign (instr2 input i))) acc (value (instr2 input i))) (set-add exec i))]
        [(equal? (instr input i) "jmp") (calc input ((eval (sign (instr2 input i))) i (value (instr2 input i))) acc (set-add exec i))]
        [(equal? (instr input i) "nop") (calc input (add1 i) acc (set-add exec i))]))