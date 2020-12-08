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
  (cond [(seen exec i) (writeln "searching") acc]
        [(= i (length input)) (writeln acc)]
        [(equal? (instr input i) "acc") (calc input (add1 i) ((eval (sign (instr2 input i)) (make-base-namespace)) acc (value (instr2 input i))) (set-add exec i))]
        [(equal? (instr input i) "jmp") (calc input ((eval (sign (instr2 input i)) (make-base-namespace)) i (value (instr2 input i))) acc (set-add exec i))]
        [(equal? (instr input i) "nop") (calc input (add1 i) acc (set-add exec i))]))

(define (find-idx input i acc)
  (cond [(= i (length input)) acc]
        [(or (equal? (instr input i) "jmp") (equal? (instr input i) "nop")) (find-idx input (add1 i) (append acc (list i)))]
        [(find-idx input (add1 i) acc)]))

(define indices (find-idx input 0 '()))

(define (swap input index)
  (cond [(equal? (instr input index) "jmp") (list-set input index (list "nop" (instr2 input index)))]
        [(equal? (instr input index) "nop") (list-set input index (list "jmp" (instr2 input index)))]))

(define (fix indices)
  (cond [(null? indices) (write "done")]
        [else (calc (swap input (car indices)) 0 0 (set)) (fix (cdr indices))]))

; part1
(calc input 0 0 (set))

; part2
(fix indices)