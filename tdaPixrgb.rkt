#lang racket
;TDA pixrgb-d
;CONSTRUCTOR
;Representación: x (int) X y (int) X r (C) X g (C) X b(C) X d (int)

(define(pixbit-d x y bit depth)
  list(x y bit depth))

;;SELECTORES(GET)

;Selector del elemento "x", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-x lista) (car lista))

;Selector del elemento "y", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-y lista) (cadr lista))