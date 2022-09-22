#lang scheme

;;Funciones Escenciales

;DOM: Int x Int x Int
;REC: Boolean
;Definición: Funcion permite verificar si un valor está dentro de un rango
(define (range? low x high)
  (if (and (number? low) (number? x) (number? high))
      (<= low x high)
      #f
      ))

;DOM: List x Int
;Rec: *
;Funcion que retorna un elemento cualquiera de una lista
;Tipo de Recursion: recursión de cola
(define (value L n)
  (if (null? L)
      #f
      (if (= n 0)
          (car L)
          (value (cdr L) (- n 1)))))