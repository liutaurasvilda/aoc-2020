#lang racket
(require (only-in math/number-theory solve-chinese))
(require rackunit)

(define input (file->lines "inputs/day13.txt"))

(define timestamp (string->number (car input)))
(define ids (map (λ (e) (string->number e)) (string-split (cadr input) ",")))

(struct bus (id t departs) #:transparent)
 
(define buses
  (map (λ (e) (bus e 0 #t)) (filter (λ (e) (not (equal? e #f))) ids)))

(define (travel b min)
  (let ([id (bus-id b)]
        [t (bus-t b)]
        [departs (bus-departs b)])
    (if (= (remainder (+ t min) id) 0)
        (bus id (+ t min) #t)
        (bus id (+ t min) #f))))

(define (depart timestamp ctime buses min)
  (let ([curr-buses (filter (λ (e) (equal? (bus-departs e) #t)) buses)])
    (cond [(and (or (> ctime timestamp) (= ctime timestamp)) (not (null? curr-buses))) 
           (* (- ctime timestamp) (bus-id (car curr-buses)))]
          [else (depart timestamp (+ ctime min) (map (λ (e) (travel e min)) buses) min)])))

; part1
(depart timestamp 0 buses 1)

; part2
(define bus-offset (make-hash))

(define (sync buses)
  (cond [(= (length buses) 1) #t]
        [else
         (let* ([b1 (first buses)]
                [b2 (second buses)]
                [offset (hash-ref bus-offset (bus-id b2))])
           (if (= (- (bus-t b2) offset) (bus-t b1)) (sync (cdr buses)) #f))]))

(define (travel2 b min)
  (let ([id (bus-id b)]
        [t (bus-t b)]
        [departs (bus-departs b)])
    (bus id (+ t min) departs)))

(define (travel-all buses min)
  (map (λ (e) (travel2 e min)) buses))

(define (departed b)
  (let* ([id (bus-id b)]
        [t (bus-t b)]
        [rem (remainder t id)])
    (bus id (- t rem) #t)))

(define (last-departure buses)
  (let* ([departures (map (λ (e) (departed e)) buses)]
        [b1 (first departures)])
    (if (= (bus-t (first departures)) (bus-t (last departures)))
        (append (list (bus (bus-id b1) (- (bus-t b1) (bus-id b1)) #t)) (cdr departures))
        departures)))

(check-equal? (last-departure (list (bus 7 1068788 #t) (bus 13 1068788 #t) (bus 59 1068788 #t) (bus 31 1068788 #t) (bus 19 1068788 #t)))
              (list (bus 7 1068781 #t) (bus 13 1068782 #t) (bus 59 1068785 #t) (bus 31 1068787 #t) (bus 19 1068788 #t)))

(check-equal? (departed (bus 3 0 #t)) (bus 3 0 #t))
(check-equal? (departed (bus 3 1 #f)) (bus 3 0 #t))
(check-equal? (departed (bus 3 3 #t)) (bus 3 3 #t))
(check-equal? (departed (bus 3 4 #f)) (bus 3 3 #t))
(check-equal? (departed (bus 3 7 #f)) (bus 3 6 #t))

(define (tour buses minutes)
  (cond [(sync (last-departure buses)) (bus-t (car (last-departure buses)))]
        [else (tour (travel-all buses minutes) minutes)]))

(call-with-values
 (λ ()
   (for/lists (as ns)
              ([(id index) (in-indexed ids)]
               #:when id)
     (values (- id index) id)))
 solve-chinese)

;7,13,x,x,59,x,31,19
;(solve-chinese '(0 12 55 25 12) '(7 13 59 31 19))