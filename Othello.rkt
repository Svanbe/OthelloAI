#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))
(require (file "Player.rkt"))
(require (file "board.rkt"))
(require (file "turning.rkt"))

;The game, start

;; When the user presse5s the quit button we want to hide the board.
(set-quit-fn! hide-board)

;; When the user presses the restart button we want to print 
;; "Restarting..." and clear the board.
(set-restart-fn! 
 (lambda () 
   (display "Restarting...")
   (newline)
   (clear-board!)
   (start-pieces)))


;; Show the user interface.
(show-board)
(start-pieces)
;(katt)

(thread black-white-loop)


