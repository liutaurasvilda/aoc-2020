#lang racket

(define input (file->lines "inputs/day7.txt"))

(define all-bags (make-hash))

(define (blobs rule)
  (map (λ (e) (string-join (list (cadr e) (caddr e))))
       (map (λ (e) (string-split e " ")) (string-split rule ", "))))

(for-each (λ (e) (hash-set! all-bags (car e) (cadr e)))
          (map (λ (e) (list (car e) (if (equal? (cadr e) "no other bags.") '() (blobs (cadr e)))))
               (map (λ (e) (string-split e " bags contain ")) input)))

(define (has-shiny bag)
  (set-member? (list->set (hash-ref all-bags bag)) "shiny gold"))

(define (empty-bag bag)
  (equal? (car bag) "other bags."))

(define (get-bags-s bag)
  (hash-ref all-bags bag))

(define (bags-of-h bags acc)
  (cond [(null? bags) acc]
        [(bags-of-h (cdr bags) (append acc (get-bags-s (car bags))))]))

(define (get-bags-m bags)
  (bags-of-h bags '()))

(define (add-to-bag k v)
  (hash-set! all-bags k (set->list (list->set (append (get-bags-s k) v)))))

(define (go keys)
  (cond [(null? keys) all-bags]
        [(add-to-bag (car keys) (get-bags-m (get-bags-s (car keys)))) (go (cdr keys))]))

(define (run bags c)
  (cond [(equal? c 3) (void)]
        [(run (hash-keys (go bags)) (add1 c))]))

(run (hash-keys all-bags) 0)

(define (iter bags result)
  (cond [(null? bags) result]
        [(if (has-shiny (car bags))
             (iter (cdr bags) (add1 result))
             (iter (cdr bags) result))]))

; part1
(iter (hash-keys all-bags) 0)

; part2
(define input2 (map (λ (e) (string-split e " bags contain ")) (file->lines "inputs/test.txt")))

(define all-bags2 (make-hash))

(define (blobs2 rule)
  (map (λ (e) (make-list (string->number (car e)) (string-append (cadr e) " " (caddr e))))
       (map (λ (e) (string-split e " ")) (string-split rule ", "))))

(for-each (λ (e) (hash-set! all-bags2 (car e) (cadr e)))
          (map (λ (e) (list (car e) (if (equal? (cadr e) "no other bags.") '() (blobs2 (cadr e))))) input2))