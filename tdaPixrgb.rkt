#lang racket
;TDA pixrgb-d
;CONSTRUCTOR
;Representación: x (int) X y (int) X r (C) X g (C) X b(C) X d (int)

(define(pixrgb-d x y r g b d)
  list(x y r g b d))

;;SELECTORES(GET)

;Selector del elemento "x", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-xRGB list) (car list))

;Selector del elemento "y", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-yRGB list) (cadr list))

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
(define(get-b lista) (car(cadddr lista)))

;Selector del elemento "d", perteneciente al TDA pixrgb
;Dominio; Lista
;Recorrido; Int
(define(get-dRGB lista) (car(car(cadddr lista))))

;PERTEENNCIA
;;Descripcion: Comprueba que el pixel ingresado sea pixbit
;;Dominio: Pixel
;;Recorrido:Boolean

(define pixrgb?
  (lambda(pixel)
    (cond
      [(empty? pixel)#f]
      [(not(equal?(length pixel)2))#f]
      [(not(equal?(length(second pixel))))#f]
      [(not(number?(first(first pixel))))#f]
      [(not(number?(second(first pixel))))#f]
      [(not(or(<=(first(second pixel))255(>=(first(second pixel))0))))#f]
      [(not(or(<=(second(second pixel))255(>=(second(second pixel))0))))#f]
      [(not(or(<=(third(second pixel))255(>=(third(second pixel))0))))#f]
      [(not(number?(fourth(second pixel))))#f]
      [else #t])))


;;MODIFICADORES(SET)
;Modificador del tda pixrgb-d de la imagen
;Dominio: Lista
;Recorrido: int

(define (set-valorxRGB pixel valor)
  (pixrgb-d valor (get-yRGB pixel) (get-r pixel) (get-g pixel) (get-b pixel) (get-dRGB pixel)))
 
(define (set-valoryRGB pixel valor)
  (pixrgb-d (get-xRGB pixel) valor (get-r pixel) (get-g pixel) (get-b pixel) (get-dRGB pixel)))
 
(define (set-valorR pixel valor)
  (pixrgb-d (get-xRGB pixel) (get-yRGB pixel) valor (get-g pixel) (get-b pixel) (get-dRGB pixel)))

(define(set-valorG pixel valor)
  (pixrgb-d (get-xRGB pixel) (get-yRGB pixel) (get-r pixel) valor (get-b pixel) (get-dRGB pixel)))

(define(set-valorB pixel valor)
  (pixrgb-d (get-xRGB pixel) (get-yRGB pixel) (get-r pixel) (get-g pixel) valor (get-dRGB pixel)))

(define(set-valorDrgb pixel valor)
  (pixrgb-d (get-xRGB pixel) (get-yRGB pixel) (get-r pixel) (get-g pixel) (get-b pixel) valor))




(provide (all-defined-out))







