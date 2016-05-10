#lang racket
(require "turning.rkt")
(require "board.rkt")
(provide (all-defined-out))

;skapar en vinnarfunktion
(define (points color)
  (if (equal? 'BLACK color)
      (hash-count *black-list*)
      (hash-count *white-list*)))

(define (winner)
  (if (> (points 'BLACK) (points 'WHITE))
      (display "Black won!")
      (display "White won!")))

(define (end-game? color move)
  (or (hash-empty? *board*) (and (null? *possible-white*) (null? *possible-black*))))


;Hanterar möjliga drag för den färgen color som skickas in.
(define (possible-moves color)
  (adds-all-possible-moves color (hash-values *board*)))

(define (adds-all-possible-moves color board-list)
  (cond ((null? board-list) '())
        ((not (list? board-list)) (if (possible-move? color board-list)
                                      (add-to-moves color board-list)
                                      '()))
        ((possible-move? color (car board-list)) (begin (add-to-moves color (car board-list))
                                                        (adds-all-possible-moves color (cdr board-list))))
        (else (adds-all-possible-moves color (cdr board-list)))))

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

(define (clean-slate)
  (begin (clear-possible-black)
         (clear-possible-white)))




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

