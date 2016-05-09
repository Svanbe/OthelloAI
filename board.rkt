#lang racket
(provide (all-defined-out))
(require racket/hash)
(require "othello-ui.rkt")

;hash-tabell med alla moves vi gjort

(define *move-list* (make-hash))

(define (get-made-move move-key)
  (hash-ref! *move-list* move-key #f))

(define (add-move! move)
  (hash-set! *move-list* move move))

(define (move-made? move)
  (hash-has-key? *move-list* move))

(define *board* (make-hash))

(define (add-to-board move)
  (hash-set! *board* move move))

(define (remove-from-board move)
  (hash-remove! *board* move))

(define (board-to-move move)
  (begin (remove-from-board move)
         (add-move! move)))

;Vi har tänkt skapa listor med de olika movsen och vilken färg de har

(define *black-list* (make-hash))
(define *white-list* (make-hash))

(define (add-black-list! move)
  (hash-set! *black-list* move move))

(define (add-white-list! move)
  (hash-set! *white-list* move move))

(define (add-color-list color move)
  (if (equal? color 'BLACK)
      (add-black-list! move)
      (add-white-list! move)))

(define (black-to-white move)
  (begin
    (hash-remove! *black-list* move)
    (add-white-list! move)))
    ;(set-white-piece-at! (car move) (cdr move))))

(define (white-to-black move)
  (begin
    (hash-remove! *white-list* move)
    (add-black-list! move)))
    ;(set-black-piece-at! (car move) (cdr move))))

(define (in-black-list? move)
  (hash-has-key? *black-list* move))

(define (in-white-list? move)
  (hash-has-key? *white-list* move))



;lägger till de fyra startbrickorna


(define (start-board x y)
  (cond ((and (= x 7) (= y 7)) (begin (add-to-board (cons x y)) '()))
        ((= x 7) (begin (add-to-board (cons x y)) (start-board 0 (+ y 1))))
        (else (begin (add-to-board (cons x y)) (start-board (+ x 1) y)))))

;får bara köras en gång! OBSOBSOBS
(start-board 0 0)


(define (start-pieces)
  (set-piece-at! 3 3 'BLACK)
  (set-piece-at! 4 4 'BLACK)
  (set-piece-at! 3 4 'WHITE)
  (set-piece-at! 4 3 'WHITE)
  (board-to-move (cons 3 3))
  (board-to-move (cons 4 4))
  (board-to-move (cons 3 4))
  (board-to-move (cons 4 3))
  (add-black-list! (cons 3 3))
  (add-black-list! (cons 4 4))
  (add-white-list! (cons 3 4))
  (add-white-list! (cons 4 3)))


;Vi tänkte lägga till grannar för att börja skapa någon form av begränsning på var man kan lägga brickorna
(define *neighbours* '())

(define (add-neighbours! move)
  (let ((x (car move))
        (y (cdr move)))
    (set! *neighbours*
          (list (cons (+ x 1) y)
                (cons (- x 1) y)
                (cons (- x 1) y)
                (cons x (+ y 1)) 
                (cons x (- y 1))   
                (cons (+ x 1) (+ y 1)) 
                (cons (- x 1) (- y 1)) 
                (cons (+ x 1) (- y 1)) 
                (cons (- x 1) (+ y 1))))))

(define (clear-neighbours)
  (set! *neighbours* '()))


(define *intersection* (make-hash))

(define (add-intersection! neighbours)
  (cond ((null? neighbours) '())
        ((move-made? (car neighbours))
         (begin (hash-set! *intersection* (car neighbours) (car neighbours))
                (add-intersection! (cdr neighbours))))
        (else (add-intersection! (cdr neighbours)))))

(define (possible-move? move)
  (begin (add-neighbours! move)
         (add-intersection! *neighbours*)
         (if (intersection-empty?) #f #t)))


;Tänkt att ta fram ett snitt mellan de grannar som finns och listan med alla moves vi gjort.

(define (intersection-empty?)
  (null? (hash-values *intersection*)))

(define (clear-intersection)
  (hash-clear! *intersection*))


;brickor i närheten

(define (neighbour-pieces move)
  (let ((x (car move))
        (y (cdr move)))
    (begin (add-neighbours! move))))





