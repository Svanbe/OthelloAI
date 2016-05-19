#lang racket
(require "othello-ui.rkt")
;(require "start-board.rkt")
(provide (all-defined-out))

(define boards%
  (class object%
    (init-field [*move-list* (make-hash)]
                [*boards* (make-hash)]
                [*black-list* (make-hash)]
                [*white-list* (make-hash)]
                [*possible-white* (make-hash)]
                [*possible-black* (make-hash)])


    
    (define/public (get-move-list)
      *move-list*)

    (define/public (get-board)
      *boards*)

    (define/public (get-white-list)
      *white-list*)

    (define/public (get-black-list)
      *black-list*)

    (define/public (get-possible-white)
      *possible-white*)

    (define/public (get-possible-black)
      *possible-black*)

(define/public (add-to-white move)
  (hash-set! *possible-white* move move))


(define/public (clear-possible-white)
  (hash-clear! *possible-white*))

(define/public (count-white-possible)
  (hash-count *possible-white*))


(define/public (add-to-black move)
  (hash-set! *possible-black* move move))

(define/public (count-black-possible)
  (hash-count *possible-black*))

(define/public (clear-possible-black)
  (hash-clear! *possible-black*))


    
    (define/public (get-made-move move-key)
      (hash-ref! *move-list* move-key #f))
    
    (define/public (add-move! move)
      (hash-set! *move-list* move move))
    
    (define/public (remove-from-move move)
      (hash-remove! *move-list* move move))
    
    (define/public (move-made? move)
      (hash-has-key? *move-list* move))
    
    (define/public (add-to-board move)
      (hash-set! *boards* move move))
    
    (define/public (remove-from-board move)
      (hash-remove! *boards* move))
    
    (define/public (clear-board)
      (hash-clear *boards*))
    
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
      ;(if (equal? this *board*)
          (begin (hash-remove! *black-list* move)
                 (add-white-list! move)
                 (set-white-piece-at! (car move) (cdr move) 'WHITE)))
          ;(begin (send this hash-remove! *black-list* move)
          ;(send this add-white-list! move))))
    
    (define/public (white-to-black move)
     ; (if (equal? this *board*)
          (begin
        (hash-remove! *white-list* move)
        (add-black-list! move)
        (set-black-piece-at! (car move) (cdr move) 'BLACK)))
         ; (begin (send this hash-remove! *white-list* move)
          ;       (send this add-black-list! move))))
    
    
    (define/public (in-black-list? move)
      (hash-has-key? *black-list* move))
    
    (define/public (in-white-list? move)
      (hash-has-key? *white-list* move))
    
    (define/public (clear-black-and-white)
      (begin (hash-clear *black-list*)
             (hash-clear *white-list*)))
    
    (super-new)))
