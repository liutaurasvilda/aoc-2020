#lang racket
(require rackunit)

(define input '(1 2 16 19 18 0))

(define (spoken numbers)
  (not (boolean? (member (last numbers) (drop-right numbers 1)))))

(check-equal? (spoken '(1 2 3)) #f)
(check-equal? (spoken '(1 2 3 1)) #t)

(define (next numbers)
  (cond [(spoken numbers) (append numbers (list (- (length numbers) (add1 (last (indexes-of (drop-right numbers 1) (last numbers)))))))]
        [else (append numbers (list 0))]))

#|
(check-equal? (next '(0 3 6)) '(0 3 6 0))
(check-equal? (next '(0 3 6 0)) '(0 3 6 0 3))
(check-equal? (next '(0 3 6 0 3)) '(0 3 6 0 3 3))
(check-equal? (next '(0 3 6 0 3 3)) '(0 3 6 0 3 3 1))
(check-equal? (next '(0 3 6 0 3 3 1)) '(0 3 6 0 3 3 1 0))
(check-equal? (next '(0 3 6 0 3 3 1 0)) '(0 3 6 0 3 3 1 0 4))
(check-equal? (next '(0 3 6 0 3 3 1 0 4)) '(0 3 6 0 3 3 1 0 4 0))
|#

(define (turn numbers c iter)
  (cond [(= c iter) (last numbers)]
        [else (turn (next numbers) (add1 c) iter)]))

(turn input (length input) 2020)

#|
(check-equal? (turn '(0 3 6) 3 2020) 436)
(check-equal? (turn '(1 3 2) 3 2020) 1)
(check-equal? (turn '(2 1 3) 3 2020) 10)
(check-equal? (turn '(1 2 3) 3 2020) 27)
(check-equal? (turn '(2 3 1) 3 2020) 78)
(check-equal? (turn '(3 2 1) 3 2020) 438)
(check-equal? (turn '(3 1 2) 3 2020) 1836)
|#