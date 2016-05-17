#lang racket
(require (file "turning.rkt"))
(require (file "AI.rkt"))
(require (file "board.rkt"))
;(require (file "Player.rkt"))
(provide (all-defined-out))
(require racket/trace)

;tar in en lista och kör ai-loop-for-evaluating på alla argz
(define (evaluate-for-ai lst not-color color)
  (cond ((null? lst) '())
        ((not (list? lst)) (ai-loop-for-evaluating not-color color lst))
        (else (begin (ai-loop-for-evaluating not-color color (car lst))
                     (evaluate-for-ai (cdr lst) not-color color)))))

(define (add-list2-to-list1 list1 list2)
  (cond ((null? list2) list1)
        ((not (list? list2)) (cons list2 list1))
        (else (add-list2-to-list1 (cons (car list2) list1) (cdr list2))))) 


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
(define (look-possibilities not-color)
  (begin (possible-moves not-color)
         (count-a-list not-color)
         (clean-slate not-color)))

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

(define (get-value key)
  (hash-ref! *move-points* key 'Fuckup))

(define (get-keys)
  (hash-keys *move-points*))

(define (clear-move-points)
  (hash-clear! *move-points*))

(define possible-move-list '())

(define (clear-possible-move-list)
  (set! possible-move-list '()))

;jämför listorna vilken position som fått minst antal poäng
;ger ut det movet vi vil göra
(define (get-best-move)
  (get-value (valuate-list (cdr (get-keys)) (car (get-keys)))))

   (define (valuate-list lst arg)
     (cond ((null? lst) arg)
           ((> arg (car lst)) (valuate-list (cdr lst) (car lst)))
           (else (valuate-list (cdr lst arg)))))
   
(define (clear-lists)
  (clear-move-points)
  (clear-possible-move-list))
   
(define (handle-lists color1 not-color1)
      (begin (possible-moves color1) ;körs för att vi ska skapa en lista med alla moves
           (add-list2-to-list1 possible-move-list (hash-values (get-color-hash color1)))
           (evaluate-for-ai possible-move-list not-color1 color1)
           (get-best-move)
           (clear-lists)))

;(trace possible-moves)