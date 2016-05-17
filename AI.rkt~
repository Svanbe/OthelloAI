#lang racket
(require "turning.rkt")
(require "board.rkt")
(provide (all-defined-out))
(require racket/trace)

;skapar en vinnarfunktion.
(define (points color)
  (if (equal? 'BLACK color)
      (hash-count *black-list*)
      (hash-count *white-list*)))

(define (winner)
  (cond ((> (points 'BLACK) (points 'WHITE)) (display "Black won!"))
        ((< (points 'BLACK) (points 'WHITE)) (display "White won!"))
        (else (display "It's a tie!"))))

(define (end-game? color)
  (begin (possible-moves 'BLACK)
         (possible-moves 'WHITE)
         (or (if (equal? color 'BLACK)
                 (null? *possible-black*)
                 (null? *possible-white*))
             (hash-empty? *board*))))

;Hanterar möjliga drag för den färgen color som skickas in.
(define (possible-moves color)
  (add-all-possible-moves color (hash-values *board*)))

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
  (begin (add-to-white move)
         (clear-turnings)))

(define (add-to-black-possible-moves move)
  (begin (add-to-black move)
         (clear-turnings)))

(define (count-a-list color)
  (if (equal? color 'WHITE)
      (count-white-possible)
      (count-black-possible)))

(define (clean-slate color)
  (if (equal? color 'BLACK)
      (clear-possible-black)
      (clear-possible-white)))

(define (get-color-hash color)
  (if (equal? color 'BLACK)
      *possible-black*
      *possible-white*))

;listorna som ska användas för AI:n när den
;ska kolla vilka möjliga drag som finns.
(define *possible-white* (make-hash))

(define (add-to-white move)
  (hash-set! *possible-white* move move))

(define (clear-possible-white)
  (hash-clear! *possible-white*))

(define (count-white-possible)
  (hash-count *possible-white*))

(define *possible-black* (make-hash))

(define (add-to-black move)
  (hash-set! *possible-black* move move))

(define (count-black-possible)
  (hash-count *possible-black*))

(define (clear-possible-black)
  (hash-clear! *possible-black*))

(trace possible-moves)
