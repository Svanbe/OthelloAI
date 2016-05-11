#lang racket
(require "turning.rkt")
(require "AI.rkt")
(require "board.rkt")
(require "Player.rkt")


;Othello AI:n
(define (activate-ai)
  (let ((color1 color)
        (not-color1 not-color))
  (begin (possible-moves color1) ;körs för att vi ska skapa en lista med alla moves
         (set! possible-move-list (hash-values (get-color-hash color1)))
         (evaluate-for-ai possible-move-list not-color1 color1))))




(define (evaluate-for-ai lst not-color color)
  (cond ((null? lst) '())
        ((not (list? lst)) (ai-loop-for-evaluating not-color color lst))
        (else (begin (ai-loop-for-evaluating not-color color (car lst))
                     (evaluate-for-ai (cdr lst) not-color color)))))


;kör loopen för att lägga till ett drag och hur många alternativ motståndaren får om man lägger där.
;not-color är definierad globalt(player) som inte color
(define (ai-loop-for-evaluating not-color color move)
  (begin (add-to-all-lists move color)
         (fake-turn color move)
         (add-to-move-points (look-possibilities not-color) move)
         (remove-from-all-lists move color)
         (turn-pieces not-color)
         (clear-turnings)))


;Lägger till i color-list, för att "låtsas" att vi gör movet.
(define (add-to-all-lists move color)
  (begin (add-color-list color move)
         (board-to-move move)))

;vänder alla brickor, för att se utfallet.
(define (fake-turn color move)
  (begin (check-pieces color move)
         (turn-pieces color)))

;tar in den andra färgen och kollar hur många drag som är möjliga.
;smart att köra efter vi gjort vårt fejkdrag.
(define (look-possibilities color)
  (begin (possible-moves color)
         (count-a-list color)
         (clean-slate color)))

;tar bort draget från alla listor vi lagt till det i.
;ställer allt till rätta helt enkelt.
(define (remove-from-all-lists move color)
  (begin (remove-color-list color move)
         (move-to-board move)))
  
;hashlistan med ett move och hur många poäng det får.
;poäng är hur många moves motståndaren får.
;dvs, ett poäng är att motståndaren kan göra ett drag nästa runda om vi lägger på platsen move.
;nyckel: poäng
;value: move

(define *move-points* (make-hash))

(define (add-to-move-points points move)
  (hash-set! *move-points* points move))

(define possible-move-list '())

(define (clear-possible-move-list)
  (set! possible-move-list '()))


  
         





         
         


         