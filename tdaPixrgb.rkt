#lang racket
;TDA pixrgb-d
;CONSTRUCTOR
;Representación: x (int) X y (int) X r (C) X g (C) X b(C) X d (int)

(define(pixbit-d x y r g b d)
  list(x y r g b d))

;;SELECTORES(GET)

;Selector del elemento "x", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-x list) (car list))

;Selector del elemento "y", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-y list) (cadr list))

;Selector del elemento "r", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-r list) (caddr list))

;Selector del elemento "g", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-g list) (cadddr list))

;Selector del elemento "b", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-b lista) (caddddr lista))

;Selector del elemento "d", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-d lista) (cadddddr lista))

;;MODIFICADORES(SET)
;Modificador del tda pixrgb-d de la imagen
;Dominio: Lista
;Recorrido: int

(define (set-valor pixel valor)
  (pixrgx-d (get-x pixel)
            (get-y pixel)
            (get-r pixel)
            (get-g pixel)
            (get-b pixel)
            valor (get-d pixel)))







