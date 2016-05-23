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
    
    
    ;tar fram de olika hasharna
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

    (define/public (get-possible-color color)
      (if (equal? color 'BLACK)
          *possible-black*
          *possible-white*))

   
    ;hanterar possible-white-list
    (define/public (add-possible-white move)
      (hash-set! *possible-white* move move))
    
    (define/public (clear-possible-white)
      (hash-clear! *possible-white*))
    
    (define/public (count-white-possible)
      (hash-count *possible-white*))
    
    ;hanterar possible-black-list
    (define/public (add-possible-black move)
      (hash-set! *possible-black* move move))
    
    (define/public (count-black-possible)
      (hash-count *possible-black*))
    
    (define/public (clear-possible-black)
      (hash-clear! *possible-black*))

    ;hantera move-list
    (define/public (get-made-move move-key)
      (hash-ref! *move-list* move-key #f))
    
    (define/public (add-move! move)
      (hash-set! *move-list* move move))
    
    (define/public (remove-from-move move)
      (hash-remove! *move-list* move))
    
    (define/public (move-made? move)
      (hash-has-key? *move-list* move))

    (define/public (clear-move-list)
      (hash-clear! *move-list*))

    ;hantera *boards*
    (define/public (add-to-board move)
      (hash-set! *boards* move move))
    
    (define/public (remove-from-board move)
      (hash-remove! *boards* move))
    
    (define/public (clear-board)
      (hash-clear! *boards*))

    ;Flyttar mellan *board* och *move-list*
    (define/public (board-to-move move)
      (begin (remove-from-board move)
             (add-move! move)))
    
    (define/public (move-to-board move)
      (begin (remove-from-move move)
             (add-to-board move)))
    
    ;Vi har tänkt skapa listor med de olika movsen och vilken färg de ha
    (define/public (add-black-list! move)
      (hash-set! *black-list* move move))
    
    (define/public (remove-black-list! move)
      (hash-remove! *black-list* move))
    
    (define/public (add-white-list! move)
      (hash-set! *white-list* move move))
    
    (define/public (remove-white-list! move)
      (hash-remove! *white-list* move))

    ;om man inte vet vilken färg det är
    (define/public (remove-color-list color move)
      (if (equal? color 'BLACK)
          (remove-black-list! move)
          (remove-white-list! move)))
    
    (define/public (add-color-list color move)
      (if (equal? color 'BLACK)
          (add-black-list! move)
          (add-white-list! move)))

    ;flyttar mellan de olika listorna
    (define/public (black-to-white move)
      (begin (hash-remove! *black-list* move)
             (add-white-list! move)
             (set-white-piece-at! (car move) (cdr move))))
    
    (define/public (white-to-black move)
      (begin
        (hash-remove! *white-list* move)
        (add-black-list! move)
        (set-black-piece-at! (car move) (cdr move))))

    (define/public (in-black-list? move)
      (hash-has-key? *black-list* move))
    
    (define/public (in-white-list? move)
      (hash-has-key? *white-list* move))
    
    (define/public (clear-black-and-white)
      (begin (hash-clear! *black-list*)
             (hash-clear! *white-list*)))
    
    (super-new)))

    ;(begin (send this hash-remove! *black-list* move)
    ;(send this add-white-list! move))))
    ; (begin (send this hash-remove! *white-list* move)
    ;       (send this add-black-list! move))))
    ; (if (equal? this *board*)
    ;(if (equal? this *board*)
    
    