#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))
(require racket/hash)
(require "board.rkt")
(provide (all-defined-out))

;what is this
(define get-x car)
(define get-y cdr)

(define *should-loop* #t)


(set-restart-fn! 
 (lambda () 
   (display "Restarting...")
   (newline)
   (clear-board!)))


;listan med balck och white

(define *pieces* '(BLACK WHITE))

;genererar varannan vit och varannan svart
(define black-or-white
  (let ((num 1))
    (lambda arg
      (begin0 (list-ref *pieces* num)
              (if (equal? num 1)
                  (set! num 0)
                  (set! num 1))))))

;visar movsen vi gör på brädet
(define (black-white-loop)
  (let ((move (get-next-move)))
    (if (move-made? move)
        (begin (display "Not possible move")
               (newline))
        (begin (board-to-move move)
               (display "got-move") 
               (display move)
               (newline)
               (when (not (eq? move 'aborted))
                 (set-piece-at! (get-x move) (get-y move)
                                (black-or-white))))))
  (black-white-loop))







