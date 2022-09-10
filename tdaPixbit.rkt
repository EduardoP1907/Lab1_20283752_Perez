#lang racket

;TDA pixbit-d
;CONSTRUCTOR
;Representación: x (int) X y (int) X bit ([0|1]) X depth (int))

(define(pixbit-d x y bit depth)
  lista(x y bit depth))

;;SELECTORES(GET)

;Selector del elemento "x", perteneciente al TDA pixbit
;Dominio; Lista
;Recorrido; Int
(define(get-x lista) (car lista))

;Selector del elemento "y", perteneciente al TDA pixbit
;Dominio; Lista
;Recorrido; Int
(define(get-y lista) (cadr lista))

;Selector del elemento "bit", perteneciente al TDA pixbit
;Dominio; Lista
;Recorrido; Int
(define(get-bit lista) (caddr lista))

;Selector del elemento "depth", perteneciente al TDA pixbit
;Dominio; Lista
;Recorrido; Int
(define(get-depth lista) (cadddr lista))

;;MODIFICADORES(SET)


