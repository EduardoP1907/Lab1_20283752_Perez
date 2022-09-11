#lang racket

;TDA pixbit-d
;CONSTRUCTOR
;Representación: x (int) X y (int) X bit ([0|1]) X depth (int))

(define(pixbit-d x y bit depth)
  list(x y bit depth))

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
;Modificaodr del tda pixbit-d de la imagen
;Dominio: Lista
;Recorrido: int

(define (set-valor pixel valor)
  (pixbit-d (get-x pixel)
            (get-y pixel)
            valor (get-depth pixel)))

