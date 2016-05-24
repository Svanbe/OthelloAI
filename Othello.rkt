#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))
(require (file "Player.rkt"))
(require (file "board-object.rkt"))
(require (file "turning.rkt"))
(require (file "Othello-procedures.rkt"))
(require (file "start-board.rkt"))
(require (file "AI-loops.rkt"))

;I denna fil spelas spelet.
;Mirjam Mattsson, Mathias Svanberg
;24/5-2016


;; When the user presses the quit button we want to hide the board.
(set-quit-fn! hide-board)

;En restartfunktion
(set-restart-fn! 
 (lambda () 
   (display "Restarting...")
   (newline)
   (clear-board!)
   (clear-board *board*)
   (clear-board *AI-board*)
   (start-pieces)))


;Startar spelet
(show-board)
(start-pieces)


(thread start-help)

