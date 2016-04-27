#lang racket
;(require racket/gui)
(require (file "othello-ui.rkt"))


(define get-x car)
(define get-y cdr)

(define *should-loop* #t)

;AI-loop etc

