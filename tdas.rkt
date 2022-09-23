#lang racket
(require "tdaPixbit.rkt")

;TDA imagen
;CONSTRUCTOR
;Representación: Ancho X Largo X [pixbit-d* |  pixrgb-d* | pixhe*]

(define(imagen ancho largo .pixel)
  list(ancho largo .pixel))

;;SELECTORES(GET)

;Selector del elemento "ancho", perteneciente al TDA imagen
;Dominio; Lista
;Recorrido; Int
(define(get-ancho list) (car list))

;Selector del elemento "largo", perteneciente al TDA imagen
;Dominio; Lista
;Recorrido; Int
(define(get-largo list) (cadr list))

;Selector del elemento "pixel", perteneciente al TDA imagen
;Dominio; Lista
;Recorrido; Int
(define(get-pixel list) (caddr list))

;;PERTENENCIA

(define bitmap?
  (lambda(imagen)
    (if(andmap pixbit? (get-pixel imagen)) #t
       #f)))
     



;;MODIFICADORES(SET)
;Modificaodr del largo de la imagen
;Dominio: Lista
;Recorrido: Lista
(define(set-largo mi-imagen new-largo)
  (imagen new-largo
          (get-ancho mi-imagen)
          (get-pixel mi-imagen)))

;Modificaodr del ancho de la imagen
;Dominio: Lista
;Recorrido: Lista
(define (set-ancho mi-imagen new-ancho)
  (imagen( get-largo mi-imagen)
         new-ancho
         (get-pixel mi-imagen)))

