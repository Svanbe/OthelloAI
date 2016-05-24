#lang racket
(require "board-object.rkt")
(require "start-board.rkt")
(provide (all-defined-out))

;En fil där vi skapar listor
;Mirjam Mattsson, Mathias Svanberg
;senast uppdaterad 24/5-2016

;potentiell lista med brickor som kan vändas
(define *potential-pieces* '())

(define (add-to-potential-pieces move)
  (set! *potential-pieces*
        (cons move *potential-pieces*)))

(define (clear-potential-pieces)
  (set! *potential-pieces* '()))


;lista med brickor som ska vändas
(define *turnings* '())

(define (add-to-turnings)
  (set! *turnings* (append *turnings* *potential-pieces*))
  (clear-potential-pieces))

(define (clear-turnings)
  (set! *turnings* '()))

(define (get-turnings)
  *turnings*)

(define (count-turnings)
  (length *turnings*))

;lägger till färdiga turns i de svarta och vita listorna
(define (turn boards turnings color)
  (cond ((null? turnings) '())
        ((equal? color 'BLACK) (begin (send boards white-to-black (car turnings))
                                      (turn boards (cdr turnings) color)))
        ((equal? color 'WHITE) (begin (send boards black-to-white (car turnings))
                                      (turn boards (cdr turnings) color)))))

;Loopar i en riktning, samt lägger till i potential pieces, sen turnings om det går.
;Annars clearar den potential-pieces
(define (check-direction boards position procedure color)
  (if (equal? color 'BLACK)
      (cond ((equal? (black-piece boards position) 'stop) (add-to-turnings))
            ((equal? (black-piece boards position) 'keep-going) (begin (add-to-potential-pieces position) (procedure boards position color)))
            (else (clear-potential-pieces)))
      (cond ((equal? (white-piece boards position) 'stop) (add-to-turnings))
            ((equal? (white-piece boards position) 'keep-going) (begin (add-to-potential-pieces position) (procedure boards position color)))
            (else (clear-potential-pieces)))))

;om det är en svart eller vit bricka vill vi ha lite olika sanningsvärden!.
;kräver *board*
(define (black-piece boards move)
  (cond ((send boards in-black-list? move) 'stop)
        ((send boards in-white-list? move) 'keep-going)
        (else 'not-possible)))

(define (white-piece boards move)
  (cond ((send boards in-black-list? move) 'keep-going)
        ((send boards in-white-list? move) 'stop)
        (else 'not-possible)))

;de olika riktningarna
(define (check-north-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons x (- y 1))))
    (check-direction boards direction check-north-direction color)))

(define (check-south-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons x (+ y 1))))
    (check-direction boards direction check-south-direction color)))

(define (check-west-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons (- x 1) y)))
    (check-direction boards direction check-west-direction color)))

(define (check-east-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons (+ x 1) y)))
    (check-direction boards direction check-east-direction color)))

(define (check-ne-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons (+ x 1) (- y 1))))
    (check-direction boards direction check-ne-direction color)))

(define (check-se-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons (+ x 1) (+ y 1))))
    (check-direction boards direction check-se-direction color)))

(define (check-nw-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons (- x 1) (- y 1))))
    (check-direction boards direction check-nw-direction color)))

(define (check-sw-direction boards move color)
  (let* ((x (car move))
         (y (cdr move))
         (direction (cons (- x 1) (+ y 1))))
    (check-direction boards direction check-sw-direction color)))

(require racket/trace)

;kör de olika riktningarna för ett move
(define (check-pieces boards color move)
  (begin (check-north-direction boards move color)
         (check-south-direction boards move color)
         (check-west-direction boards move color)
         (check-east-direction boards move color)
         (check-ne-direction boards move color)
         (check-nw-direction boards move color)
         (check-se-direction boards move color)
         (check-sw-direction boards move color)))


;vänder bitarna 
(define (turn-pieces boards color)
      (turn boards *turnings* color))

;Kollar om det är ett möjligt drag 
(define (possible-move? boards color move)
  (begin (check-pieces boards color move)
         (not (null? *turnings*))))

