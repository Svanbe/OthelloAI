#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))

;Player loop

(define get-x car)
(define get-y cdr)

(define *should-loop* #t)

;; Loop, until the user presses quit, and read in a move and
;; set the piece at that board position to a randomly choosen
;;  piece. Also print the coordinates of the move.

(define (put-player-piece-loop)
  (let ((move (get-next-move)))
    (display "got-move ") 
    (display move)
    (newline)
    (when (not (eq? move 'aborted))
      (set-piece-at! (get-x move) (get-y move)
                     'WHITE)))
  (put-player-piece-loop))

(provide (all-defined-out))