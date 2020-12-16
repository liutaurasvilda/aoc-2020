#lang racket
(require srfi/13)

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

(define (build-range start end)
  (sequence->list (in-range start (add1 end))))

(define departure-location (append (build-range 44 401) (build-range 415 965)))
(define departure-station (append (build-range 44 221) (build-range 243 953)))
(define departure-platform (append (build-range 29 477) (build-range 484 963)))
(define departure-track (append (build-range 43 110) (build-range 126 951)))
(define departure-date (append (build-range 48 572) (build-range 588 965)))
(define departure-time (append (build-range 48 702) (build-range 719 955)))

(define arrival-location (append (build-range 35 336) (build-range 358 960)))
(define arrival-station (append (build-range 47 442) (build-range 449 955)))
(define arrival-platform (append (build-range 25 632) (build-range 639 970)))
(define arrival-track (append (build-range 34 461) (build-range 472 967)))
(define class (append (build-range 41 211) (build-range 217 959)))
(define duration (append (build-range 29 500) (build-range 519 969)))
(define price (append (build-range 39 423) (build-range 440 969)))
(define route (append (build-range 50 264) (build-range 282 958)))
(define row (append (build-range 50 907) (build-range 920 972)))
(define seat (append (build-range 27 294) (build-range 315 954)))
(define train (append (build-range 29 813) (build-range 827 962)))
(define type (append (build-range 45 531) (build-range 546 956)))
(define wagon (append (build-range 29 283) (build-range 292 957)))
(define zone (append (build-range 45 518) (build-range 525 974)))

(define invalid-numbers '(986 999 977 5 982 979 985 983 992 995 994 2 987 978 8 19 976 12 16 17 4 18 990 13 975 23 20 22 996 997))

(define nearby (map (λ (e) (map string->number (string-split e ","))) (file->lines "inputs/other-tickets.txt")))

(define (valid ticket invalid-numbers)
  (cond [(null? invalid-numbers) #t]
        [(not (member (car invalid-numbers) ticket)) (valid ticket (cdr invalid-numbers))]
        [else #f]))

(define valid-tickets (filter (λ (e) (valid e invalid-numbers)) nearby))

(define (allmatch range tickets pos)
  (cond [(null? tickets) #t]
        [(member (list-ref (car tickets) pos) range) (allmatch range (cdr tickets) pos)]
        [else #f]))

(define (possible-pos range tickets pos acc)
  (cond [(= pos (length (car tickets))) acc]
        [(allmatch range tickets pos) (possible-pos range tickets (add1 pos) (append acc (list pos)))]
        [else (possible-pos range tickets (add1 pos) acc)]))


(write "departure-location")(possible-pos departure-location valid-tickets 0 '())
(write "departure-station")(possible-pos departure-station valid-tickets 0 '())
(write "departure-platform")(possible-pos departure-platform valid-tickets 0 '())
(write "departure-track")(possible-pos departure-track valid-tickets 0 '())
(write "departure-date")(possible-pos departure-date valid-tickets 0 '())
(write "departure-time")(possible-pos departure-time valid-tickets 0 '())
(write "arrival-location")(possible-pos arrival-location valid-tickets 0 '())
(write "arrival-station")(possible-pos arrival-station valid-tickets 0 '())
(write "arrival-platform")(possible-pos arrival-platform valid-tickets 0 '())
(write "arrival-track")(possible-pos arrival-track valid-tickets 0 '())
(write "class")(possible-pos class valid-tickets 0 '())
(write "duration")(possible-pos duration valid-tickets 0 '())
(write "price")(possible-pos price valid-tickets 0 '())
(write "route")(possible-pos route valid-tickets 0 '())
(write "row")(possible-pos row valid-tickets 0 '())                ; 1
(write "seat")(possible-pos seat valid-tickets 0 '())
(write "train")(possible-pos train valid-tickets 0 '())
(write "type")(possible-pos type valid-tickets 0 '())
(write "wagon")(possible-pos wagon valid-tickets 0 '())
(write "zone")(possible-pos zone valid-tickets 0 '())

; part2
(apply * '(127 83 103 73 71 131))