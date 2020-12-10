#lang racket

(define in (sort (map (λ (e) (string->number e)) (file->lines "inputs/day10.txt")) <))
(define built-in (+ (apply max in) 3))
(define input (append in (list built-in)))

(define (diff prev curr)
  (- curr prev))

(define (jolt input prev ones threes)
  (cond [(null? input) (* ones threes)]
        [(cond
           [(= (diff prev (car input)) 1) (jolt (cdr input) (car input) (add1 ones) threes)]
           [(= (diff prev (car input)) 3) (jolt (cdr input) (car input) ones (add1 threes))])]))

(define 1-perm 1) ; [1]
(define 2-perm 1) ; [1 2]
(define 3-perm 2) ; [1 2 3] [1 3]
(define 4-perm 4) ; [1 2 3 4] [1 3 4] [1 2 4] [1 4]
(define 5-perm 7) ; [1 2 3 4 5] [1 3 4 5] [1 2 4 5] [1 2 3 5] [1 4 5] [1 2 5] [1 3 5]

(define (perm l)
  (cond [(= (length l) 1) 1-perm]
        [(= (length l) 2) 2-perm]
        [(= (length l) 3) 3-perm]
        [(= (length l) 4) 4-perm]
        [(= (length l) 5) 5-perm]))

(define (append-to-last l n)
  (list-set l (- (length l) 1) (append (last l) (list n))))

(define (decompose input result)
  (cond [(null? input) result]
        [(= (- (car input) 1) (last (last result))) (decompose (cdr input) (append-to-last result (car input)))]
        [else (decompose (cdr input) (append result (list (list (car input)))))]))

; part1
(jolt input 0 0 0)

; part2
(apply * (map (λ (e) (perm e)) (decompose input (list (list 0)))))