#lang racket

(define input
  (map (λ (e)
         (list (substring e 0 1) (string->number (substring e 1))))
       (file->lines "inputs/day12.txt")))

(define (move current units)
  (list (car current) (+ (cadr current) units)))

(define (move2 current units)
  (cond [(equal? (car current) "N")
         (cond
           [(> units (cadr current)) (list "S" (abs (- units (cadr current))))]
           [(< units (cadr current)) (list "N" (abs (- units (cadr current))))]
           [(= units (cadr current)) (list "N" (abs (- units (cadr current))))])]
        [(equal? (car current) "S")
         (cond
           [(> units (cadr current)) (list "N" (abs (- units (cadr current))))]
           [(< units (cadr current)) (list "S" (abs (- units (cadr current))))]
           [(= units (cadr current)) (list "S" (abs (- units (cadr current))))])]
        [(equal? (car current) "E")
         (cond
           [(> units (cadr current)) (list "W" (abs (- units (cadr current))))]
           [(< units (cadr current)) (list "E" (abs (- units (cadr current))))]
           [(= units (cadr current)) (list "E" (abs (- units (cadr current))))])]
        [(equal? (car current) "W")
         (cond
           [(> units (cadr current)) (list "E" (abs (- units (cadr current))))]
           [(< units (cadr current)) (list "W" (abs (- units (cadr current))))]
           [(= units (cadr current)) (list "W" (abs (- units (cadr current))))])]))

(define (turn current direction)
  (cond 
    [(equal? current "N")
     (cond
       [(equal? (car direction) "L")
        (cond
          [(= (cadr direction) 90) "W"]
          [(= (cadr direction) 180) "S"]
          [(= (cadr direction) 270) "E"]
          [(= (cadr direction) 360) "N"])]
       [(equal? (car direction) "R")
        (cond
          [(= (cadr direction) 90) "E"]
          [(= (cadr direction) 180) "S"]
          [(= (cadr direction) 270) "W"]
          [(= (cadr direction) 360) "N"])])]
    [(equal? current "S")
     (cond
       [(equal? (car direction) "L")
        (cond
          [(= (cadr direction) 90) "E"]
          [(= (cadr direction) 180) "N"]
          [(= (cadr direction) 270) "W"]
          [(= (cadr direction) 360) "S"])]
       [(equal? (car direction) "R")
        (cond
          [(= (cadr direction) 90) "W"]
          [(= (cadr direction) 180) "N"]
          [(= (cadr direction) 270) "E"]
          [(= (cadr direction) 360) "S"])])]
    [(equal? current "E")
     (cond
       [(equal? (car direction) "L")
        (cond
          [(= (cadr direction) 90) "N"]
          [(= (cadr direction) 180) "W"]
          [(= (cadr direction) 270) "S"]
          [(= (cadr direction) 360) "E"])]
       [(equal? (car direction) "R")
        (cond
          [(= (cadr direction) 90) "S"]
          [(= (cadr direction) 180) "W"]
          [(= (cadr direction) 270) "N"]
          [(= (cadr direction) 360) "E"])])]
    [(equal? current "W")
     (cond
       [(equal? (car direction) "L")
        (cond
          [(= (cadr direction) 90) "S"]
          [(= (cadr direction) 180) "E"]
          [(= (cadr direction) 270) "N"]
          [(= (cadr direction) 360) "W"])]
       [(equal? (car direction) "R")
        (cond
          [(= (cadr direction) 90) "N"]
          [(= (cadr direction) 180) "E"]
          [(= (cadr direction) 270) "S"]
          [(= (cadr direction) 360) "W"])])]))

(define (nav input direction ew ns)
  (cond [(null? input) (+ (abs (cadr ew)) (abs (cadr ns)))]

        [(or (and (equal? (car (car input)) "N") (equal? (car ns) "N")) (and (equal? (car (car input)) "S") (equal? (car ns) "S")))
         (nav (cdr input) direction ew (move ns (cadr (car input))))]
        [(or (equal? (car (car input)) "N") (equal? (car (car input)) "S"))
         (nav (cdr input) direction ew (move2 ns (cadr (car input))))]

        [(or (and (equal? (car (car input)) "E") (equal? (car ew) "E")) (and (equal? (car (car input)) "W") (equal? (car ew) "W")))
         (nav (cdr input) direction (move ew (cadr (car input))) ns)]
        [(or (equal? (car (car input)) "E") (equal? (car (car input)) "W"))
         (nav (cdr input) direction (move2 ew (cadr (car input))) ns)]

        [(or (equal? (car (car input)) "L") (equal? (car (car input)) "R"))
         (nav (cdr input) (turn direction (car input)) ew ns)]

        [(equal? (car (car input)) "F")
         (cond
           [(and (or (equal? direction "E") (equal? direction "W")) (equal? direction (car ew)))
            (nav (cdr input) direction (move ew (cadr (car input))) ns)]
           [(or (equal? direction "E") (equal? direction "W"))
            (nav (cdr input) direction (move2 ew (cadr (car input))) ns)]
           [(and (or (equal? direction "N") (equal? direction "S")) (equal? direction (car ns)))
            (nav (cdr input) direction ew (move ns (cadr (car input))))]
           [(or (equal? direction "N") (equal? direction "S"))
            (nav (cdr input) direction ew (move2 ns (cadr (car input))))])]))

; part1
(nav input "E" '("E" 0) '("N" 0))

(define (turn-waypoint waypoint direction)
  (cond [(and (or (equal? (turn (first waypoint) direction) "E") (equal? (turn (first waypoint) direction) "W")) (or (equal? (first waypoint) "E") (equal? (first waypoint) "W")))
         (list (turn (first waypoint) direction) (second waypoint) (turn (third waypoint) direction) (fourth waypoint))]
        [(or (equal? (turn (first waypoint) direction) "E") (equal? (turn (first waypoint) direction) "W"))
         (list (turn (third waypoint) direction) (fourth waypoint) (turn (first waypoint) direction) (second waypoint))]
        [(and (or (equal? (turn (first waypoint) direction) "N") (equal? (turn (first waypoint) direction) "S")) (or (equal? (first waypoint) "N") (equal? (first waypoint) "S")))
         (list (turn (first waypoint) direction) (second waypoint) (turn (third waypoint) direction) (fourth waypoint))]
        [(or (equal? (turn (first waypoint) direction) "N") (equal? (turn (first waypoint) direction) "S"))
         (list (turn (third waypoint) direction) (fourth waypoint) (turn (first waypoint) direction) (second waypoint))]))

(define (move-waypoint waypoint direction)
  (cond [(and (equal? (car direction) "N") (equal? (third waypoint) "N"))
         (append (list (first waypoint) (second waypoint)) (move (list (third waypoint) (fourth waypoint)) (second direction)))]
        [(equal? (car direction) "N")
         (append (list (first waypoint) (second waypoint)) (move2 (list (third waypoint) (fourth waypoint)) (second direction)))]

        [(and (equal? (car direction) "S") (equal? (third waypoint) "S"))
         (append (list (first waypoint) (second waypoint)) (move (list (third waypoint) (fourth waypoint)) (second direction)))]
        [(equal? (car direction) "S")
         (append (list (first waypoint) (second waypoint)) (move2 (list (third waypoint) (fourth waypoint)) (second direction)))]

        [(and (equal? (car direction) "E") (equal? (first waypoint) "E"))
         (append (move (list (first waypoint) (second waypoint)) (second direction)) (list (third waypoint) (fourth waypoint)))]
        [(equal? (car direction) "E")
         (append (move2 (list (first waypoint) (second waypoint)) (second direction)) (list (third waypoint) (fourth waypoint)))]

        [(and (equal? (car direction) "W") (equal? (first waypoint) "W"))
         (append (move (list (first waypoint) (second waypoint)) (second direction)) (list (third waypoint) (fourth waypoint)))]
        [(equal? (car direction) "W")
         (append (move2 (list (first waypoint) (second waypoint)) (second direction)) (list (third waypoint) (fourth waypoint)))]))

(define (move-ship waypoint ship times) 
  (let ([ew (if (equal? (first waypoint) (first ship))
                (move (list (first ship) (second ship)) (* (second waypoint) times))
                (move2 (list (first ship) (second ship)) (* (second waypoint) times)))]
        [ns (if (equal? (third waypoint) (third ship))
                (move (list (third ship) (fourth ship)) (* (fourth waypoint) times))
                (move2 (list (third ship) (fourth ship)) (* (fourth waypoint) times)))])
    (append ew ns)))
  
(define (nav2 input waypoint ship)
  (cond [(null? input) (+ (second ship) (fourth ship))]
        [(or (equal? (car (car input)) "N") (equal? (car (car input)) "S") (equal? (car (car input)) "E") (equal? (car (car input)) "W"))
         (nav2 (cdr input) (move-waypoint waypoint (car input)) ship)]
        [(or (equal? (car (car input)) "L") (equal? (car (car input)) "R"))
         (nav2 (cdr input) (turn-waypoint waypoint (car input)) ship)]
        [(equal? (car (car input)) "F")
         (nav2 (cdr input) waypoint (move-ship waypoint ship (cadr (car input))))]))

; part2
(nav2 input '("E" 10 "N" 1) '("E" 0 "N" 0))