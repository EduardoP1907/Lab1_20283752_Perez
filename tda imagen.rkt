#lang racket

;TDA imagen
;Representación: Ancho X Largo X [pixbit-d* |  pixrgb-d* | pixhe*]

(define(imagen ancho largo .arg)
  list(ancho largo .arg))


