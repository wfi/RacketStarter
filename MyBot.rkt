#lang racket

;; MyBot.rkt
;;
;; With thanks to the Common-Lisp starter kit

(require "ants.rkt")


(define (do-turn)
  (for-each (lambda (ant)
              (ormap (lambda (d)
                       (let ([nl (send *STATE* new-location (car ant) (cdr ant) d)])
                         (if (send *STATE* unoccupied? nl)
                             (begin (send *STATE* issue-order (car ant) (cdr ant) d)
                                    #t)
                             #f)))
                     '(#:north #:east #:south #:west)))
            (get-field my-ants *STATE*)))

;; 
(define (main)
  (do () ((send *STATE* parse-game-state))
    (do-turn)
    (finish-turn)
    ))

(main)