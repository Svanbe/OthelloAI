#lang racket
(define AI-board%
  (class object%
    (init-field [*move-list* (make-hash)]
                [*board* (make-hash)]
                [*black-list* (make-hash)]
                [*white-list* (make-hash)])      

(define/public (get-made-move move-key)
  (hash-ref! *move-list* move-key #f))

(define/public (add-move! move)
  (hash-set! *move-list* move move))

(define/public (remove-from-move move)
  (hash-remove! *move-list* move move))

(define/public (move-made? move)
  (hash-has-key? *move-list* move))

(define/public (add-to-board move)
  (hash-set! *board* move move))

(define/public (remove-from-board move)
  (hash-remove! *board* move))

(define/public (clear-board)
  (hash-clear *board*))

(define/public (board-to-move move)
  (begin (remove-from-board move)
         (add-move! move)))

(define/public (move-to-board move)
  (begin (remove-from-move move)
         (add-to-board move)))

;Vi har tänkt skapa listor med de olika movsen och vilken färg de har

(define/public (add-black-list! move)
  (hash-set! *black-list* move move))

(define/public (remove-black-list! move)
  (hash-remove! *black-list* move))

(define/public (add-white-list! move)
  (hash-set! *white-list* move move))

(define/public (remove-white-list! move)
  (hash-remove! *white-list* move)) 

(define/public (remove-color-list color move)
  (if (equal? color 'BLACK)
      (remove-black-list! move)
      (remove-white-list! move)))

(define/public (add-color-list color move)
  (if (equal? color 'BLACK)
      (add-black-list! move)
      (add-white-list! move)))

(define/public (black-to-white move)
  (begin
    (hash-remove! *black-list* move)
    (add-white-list! move)))

(define/public (white-to-black move)
  (begin
    (hash-remove! *white-list* move)
    (add-black-list! move)))

(define/public (in-black-list? move)
  (hash-has-key? *black-list* move))

(define/public (in-white-list? move)
  (hash-has-key? *white-list* move))

(define/public (clear-black-and-white)
  (begin (hash-clear *black-list*)
         (hash-clear *white-list*)))
