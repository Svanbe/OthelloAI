#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))
(require "board-object.rkt")
(require "start-board.rkt")
(require "turning.rkt")
(require "AI.rkt")
(require (file "othello-AI.rkt"))
(provide (all-defined-out))
(require racket/trace)


;what is this.
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

(define color 'BLACK)

(define (not-color)
  (if (equal? color 'BLACK)
      'WHITE
      'BLACK))

;visar movsen vi gör på brädet
(define (black-white-loop boards)
  (let ((move (get-next-move)))
    (begin
      (cond ((end-game? color) (winner))
            ((or (send *board* move-made? move) (not (possible-move? color move)))
             (begin (display "Not possible move") (newline)))
            (else (begin
                    (send boards board-to-move move)
                    (send boards add-color-list color move)
                    (turn-pieces boards color)
                    (clear-turnings)
                    (clean-slate 'BLACK)
                    (clean-slate 'WHITE)
                    (display "got-move") 
                    (display move)
                    (newline)
                    (when (not (eq? move 'aborted))
                      (set-piece-at! (get-x move) (get-y move)
                                     color))
                    (set! color (black-or-white)))))))
  (black-white-loop boards))

;Othello AI:n
(define (activate-ai)
    (handle-lists color (not-color)))

(define (play-game-loop boards)
  (let ((turn 1))
    (if (equal? turn 1)
        (when (black-white-loop (get-next-move)) (set! turn 0))
        (when (black-white-loop (activate-ai)) (set! turn 1)))))

