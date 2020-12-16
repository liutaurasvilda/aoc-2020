#lang racket
(require rackunit)

; part 1
(define input '(1 2 16 19 18 0))

(define (spoken numbers)
  (not (boolean? (member (last numbers) (drop-right numbers 1)))))

(define (next numbers)
  (cond [(spoken numbers) (append numbers (list (- (length numbers) (add1 (last (indexes-of (drop-right numbers 1) (last numbers)))))))]
        [else (append numbers (list 0))]))

(define (turn numbers c iter)
  (cond [(= c iter) (last numbers)]
        [else (writeln (next numbers)) (turn (next numbers) (add1 c) iter)]))

;(turn input (length input) 2020)

; part 2
(define numbers (make-hash))
(hash-set! numbers 1 (list 1))
(hash-set! numbers 2 (list 2))
(hash-set! numbers 16 (list 3))
(hash-set! numbers 19 (list 4))
(hash-set! numbers 18 (list 5))
(hash-set! numbers 0 (list 6))

(define (spoken2 n)
  (and (hash-has-key? numbers n) (= (length (hash-ref numbers n)) 2)))

(define (next2 last c)
  (cond [(spoken2 last)
         (let ([n (abs (apply - (hash-ref numbers last)))])
           (cond [(spoken2 n) (hash-set! numbers n (list (cadr (hash-ref numbers n)) c)) n]
                 [(hash-has-key? numbers n) (hash-set! numbers n (append (hash-ref numbers n) (list c))) n]
                 [else (hash-set! numbers n (list c)) n]))]
        [else (let ([n1 0])
                (cond [(spoken2 n1) (hash-set! numbers n1 (list (cadr (hash-ref numbers n1)) c)) n1]
                      [(hash-has-key? numbers n1) (hash-set! numbers n1 (append (hash-ref numbers n1) (list c))) n1]
                      [else (hash-set! numbers n1 (list c)) n1]))]))

(define (turn2 last c iter)
  (cond [(= c (add1 iter)) last]
        [else (turn2 (next2 last c) (add1 c) iter)]))

; part 2
(turn2 0 7 30000000)