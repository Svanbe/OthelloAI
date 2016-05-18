#lang racket
(require "turning.rkt")
(require "board-object.rkt")
(require "start-board.rkt")
(provide (all-defined-out))
(require racket/trace)

;skapar en vinnarfunktion.
(define (points color)
  (cond ((equal? 'BLACK color) (hash-count (send *board* get-black-list)))
        ((equal? 'WHITE color) (hash-count (send *board* get-white-list)))
        (else #f)))

(define (winner)
  (cond ((> (points 'BLACK) (points 'WHITE)) (display "Black won!"))
        ((< (points 'BLACK) (points 'WHITE)) (display "White won!"))
        (else (display "It's a tie!"))))

(define (end-game? color)
  (begin (possible-moves 'BLACK)
         (possible-moves 'WHITE)
         (or (if (equal? color 'BLACK)
                 (null? (send *board* get-possible-black))
                 (null? (send *board* get-possible-white)))
             (hash-empty? (send *board* get-board)))))

;Hanterar möjliga drag för den färgen color som skickas in.
(define (possible-moves color)
  (add-all-possible-moves color (hash-values (send *board* get-board))))

(define (add-all-possible-moves color board-list)
  (cond ((null? board-list) '())
        ((not (list? board-list)) (if (possible-move? color board-list)
                                      (add-to-moves color board-list)
                                      '()))
        ((possible-move? color (car board-list)) (begin (add-to-moves color (car board-list))
                                                        (add-all-possible-moves color (cdr board-list))))
        (else (add-all-possible-moves color (cdr board-list)))))

(define (add-to-moves color move)
  (if (equal? color 'WHITE)
      (add-to-white-possible-moves move)
      (add-to-black-possible-moves move)))

(define (add-to-white-possible-moves move)
  (begin (send *board* add-to-white move)
         (clear-turnings)))

(define (add-to-black-possible-moves move)
  (begin (send *board* add-to-black move)
         (clear-turnings)))

(define (count-a-list color)
  (if (equal? color 'WHITE)
      (send *board* count-white-possible)
      (send *board* count-black-possible)))

(define (clean-slate color)
  (if (equal? color 'BLACK)
      (send *board* clear-possible-black)
      (send *board* clear-possible-white)))

(define (get-color-hash color)
  (if (equal? color 'BLACK)
      (send *board* get-possible-black)
      (send *board* get-possible-white)))

;listorna som ska användas för AI:n när den
;ska kolla vilka möjliga drag som finns.

