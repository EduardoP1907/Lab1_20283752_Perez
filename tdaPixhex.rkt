#lang racket
;TDA pixhex-d
;CONSTRUCTOR
;Representación: x (int) X y (int) X hex(String) X d (int)

(define(pixhex-d x y hex d)
  list(x y hex d))

;;SELECTORES(GET)

;Selector del elemento "x", perteneciente al TDA pixhex-d
;Dominio; Lista
;Recorrido; Int
(define(get-x list) (car list))

;Selector del elemento "y", perteneciente al TDA pixhex-d
;Dominio; Lista
;Recorrido; Int
(define(get-y list) (cadr list))

;Selector del elemento "hex", perteneciente al TDA pixhex-d
;Dominio; Lista
;Recorrido; Int
(define(get-hex list) (caddr list))

;Selector del elemento "d", perteneciente al TDA pixhex-d
;Dominio; Lista
;Recorrido; Int
(define(get-d list) (cadddr list))


;;MODIFICADORES(SET)
;Modificador del tda pixhex-d de la imagen
;Dominio: Lista
;Recorrido: int|string

(define (set-valor pixel valor)
  (pixhex-d (get-x pixel)
            (get-y pixel)
            (get-hex pixel)
            valor (get-d pixel)))




