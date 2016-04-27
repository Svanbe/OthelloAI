#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))
(require racket/hash)

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
    (if (not (false? (get-made-move move)))
        (begin (display "Not possible move")
               (newline))
        (begin (add-move! move)
               (add-color-list move)
               (display "got-move") 
               (display move)
               (newline)
               (when (not (eq? move 'aborted))
                 (set-piece-at! (get-x move) (get-y move)
                                (black-or-white))))))
  (black-white-loop))

;hash-tabell med alla moves vi gjort

(define *move-list* (make-hash))

(define (get-made-move move-key)
  (hash-ref! *move-list* move-key #f))

(define (add-move! move)
  (hash-set! *move-list* move move))

;Vi har tänkt skapa listor med de olika movsen och vilken färg de har

(define *black-list* (make-hash))
(define *white-list* (make-hash))

(define (add-black-list! move)
  (hash-set! *black-list* move move))

(define (add-white-list! move)
  (hash-set! *white-list* move move))

(define (add-color-list move)
  (if (equal? (black-or-white) 'BLACK)
      (add-black-list! move)
      (add-white-list! move)))


;lägger till de fyra startbrickorna

(define (start-pieces)
  (set-piece-at! 3 3 'BLACK)
  (set-piece-at! 4 4 'BLACK)
  (set-piece-at! 3 4 'WHITE)
  (set-piece-at! 4 3 'WHITE)
  (add-move! (cons 3 3))
  (add-move! (cons 4 4))
  (add-move! (cons 3 4))
  (add-move! (cons 4 3)))


;Vi tänkte lägga till grannar för att börja skapa någon form av begränsning på var man kan lägga brickorna
(define *neighbours* (make-hash))

(define (add-neighbours! move)
  (let ((x (car move))
        (y (cdr move)))
    (hash-set! *neighbours* (cons (+ x 1) y) (cons (+ x 1) y))
    (hash-set! *neighbours* (cons (- x 1) y) (cons (- x 1) y))
    (hash-set! *neighbours* (cons x (+ y 1)) (cons x (+ y 1)))
    (hash-set! *neighbours* (cons x (- y 1)) (cons x (- y 1)))  
    (hash-set! *neighbours* (cons (+ x 1) (+ y 1)) (cons (+ x 1) (+ y 1)))
    (hash-set! *neighbours* (cons (- x 1) (- y 1)) (cons (- x 1) (- y 1)))
    (hash-set! *neighbours* (cons (+ x 1) (- y 1)) (cons (+ x 1) (- y 1)))
    (hash-set! *neighbours* (cons (- x 1) (+ y 1)) (cons (- x 1) (+ y 1)))))

(define (clear-neighbours)
  (hash-clear! *neighbours*))


(define *intersection* (make-hash))

;(define (add-intersection!)
 ; (hash-union! *neighbours* *move-list*))

  
  ;(if (cond ((hash-has-key? *neighbours* (car (hash-values *move-list*)))
   ;         (hash-set! *intersection* (car (hash-values *move-list*)) (car (hash-values *move-list*)))))))



;Tänkt att ta fram ett snitt mellan de grannar som finns och listan med alla moves vi gjort.

(define (intersection-empty?)
  (null? (hash-values *intersection*)))

(define (clear-intersection)
  (hash-clear! *intersection*))

;(define (katt)
 ; (when (black-white-loop)
  ;  (add-neighbours! (get-next-move))
   ; (add-intersection! (get-next-move))))



