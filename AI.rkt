#lang racket
(require "turning.rkt")
(require "board-object.rkt")
(require "start-board.rkt")
(provide (all-defined-out))
(require racket/trace)


;Hanterar möjliga drag för den färgen color som skickas in.
(define (possible-moves boards color)
  (add-all-possible-moves boards color (hash-values (send boards get-board-list))))

(define (add-all-possible-moves boards color board-list)
  (cond ((null? board-list) '())
        ((not (list? board-list)) (if (possible-move? boards color board-list)
                                      (add-to-moves boards color board-list)
                                      '()))
        ((possible-move? boards color (car board-list)) (begin (add-to-moves boards color (car board-list))
                                                        (add-all-possible-moves boards color (cdr board-list))))
        (else (add-all-possible-moves boards color (cdr board-list)))))


;hanterar listorna för possible-moves
(define (add-to-moves boards color move)
  (if (equal? color 'WHITE)
      (add-to-white-possible boards move)
      (add-to-black-possible boards move)))

(define (add-to-white-possible boards move)
  (begin (send boards add-possible-white move)
         (clear-turnings)))

(define (add-to-black-possible boards move)
  (begin (send boards add-possible-black move)
         (clear-turnings)))

(define (count-a-list boards color)
  (if (equal? color 'WHITE)
      (send boards count-white-possible)
      (send boards count-black-possible)))

(define (clean-slate boards color)
  (if (equal? color 'BLACK)
      (send boards clear-possible-black)
      (send boards clear-possible-white)))

(define (get-color-hash boards color)
  (if (equal? color 'BLACK)
      (send boards get-possible-black)
      (send boards get-possible-white)))

;listorna som ska användas för AI:n när den
;ska kolla vilka möjliga drag som finns.

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

