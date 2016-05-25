#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))
(require "board-object.rkt")
(require "start-board.rkt")
(require "turning.rkt")
(require (file "othello-procedures.rkt"))
(require "AI-loops.rkt")
(provide (all-defined-out))

;En fil där vi definierar loopen black-white-loop som är den
;loop som körs hela tiden i Othello.rkt. 
;används på move - car ger x-koordinaten och cdr ger y-koordinaten.
(define get-x car)
(define get-y cdr)

(define *should-loop* #t)

;restart-fn som inte funkar 
(set-restart-fn! 
 (lambda () 
   (display "Restarting...")
   (newline)
   (clear-board!)))

;vi börjar med att definea color som 'BLACK
;för att när ett drag sedan är gjort set! color to not-color
(define color 'BLACK)

(define (not-color)
  (if (equal? color 'BLACK)
      'WHITE
      'BLACK))

;på samma sätt som color så definear vi current-board som
;*board* (huvudbrädet) för att sedan vid AI köra varannan
;*board* *AI-board*.
(define current-board *board*)

(define (not-board)
  (if (equal? current-board *board*)
      *AI-board*
      *board*))

;visar movsen vi gör på brädet
(define (black-white-loop boards)
  (if (end-game? color)
      (begin (winner) (set! option 'end-loop))
      (begin 
  (let ((move (if (equal? boards *board*)
                  (AI option color);(get-next-move)
                  (AI option color))))
    (begin
      (cond ((or (not (possible-move? *board* color move)) (send *board* move-made? move))
             (begin (display "Not possible move") (newline)))
            (else (begin (make-the-move move *board* color)
                         (clear-turnings)
                         (display "got-move")
                         (display move)
                         (newline)
                         (set-piece-at! (get-x move) (get-y move)
                                        color)
                         (set! color (not-color))
                         (set! current-board (not-board)))))))))
  (clear-possible-AI)
  (send *board* clear-both-b-w)
  (start option))

(define (start-help)
  (start option))

;beroende på vad  vi tar in som option så kör vi antingen
;2-players, simple-AI eller complex-AI.
(define (start option)
  (cond ((equal? option '2-players) (black-white-loop *board*))
        ((equal? option 'simple-AI) (black-white-loop current-board))
        ((equal? option 'complex-AI) (black-white-loop current-board))))

;beroende på om det är simple-AI eller complex-AI
;hämtas olika moves.
(define (AI option color)
  (if (equal? option 'simple-AI)
      (simple-AI color)
      (complex-AI color)))

(define option 'complex-AI)



