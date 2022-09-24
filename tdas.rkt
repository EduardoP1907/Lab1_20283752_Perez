#lang racket
(require "tdaPixbit.rkt")
(require "tdaPixrgb.rkt")
(require "tdaPixhex.rkt")

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

(define pixmap?
  (lambda(imagen)
    (if(andmap pixrgb?(get-pixel imagen)) #t
       #f)))

(define hexmap?
  (lambda(imagen)
    (if(andmap pixhex?(get-pixel imagen)) #t
       #f)))


;;MODIFICADORES(SET)
;Modificaodr del largo de la imagen
;Dominio: imagen
;Recorrido: imagen
(define(set-largo mi-imagen new-largo)
  (imagen new-largo
          (get-ancho mi-imagen)
          (get-pixel mi-imagen)))

(define (flipH-rec ancho pixel modxpixel)
  (if (null? (cdr pixel))
      (list (modxpixel [car pixel] [- ancho (car pixel)] ))
      (append (list (modxpixel [car pixel] [- ancho (car pixel)] )) (flipH-rec ancho (cdr pixel) modxpixel))))
;;MODIFICADORES(SET)
;Modificaodr que permite invertir una imagen horizontalmente
;Dominio: imagen
;Recorrido: imagen
(define (flipH imagen)
  (cond
    [(pixmap? imagen) (list (get-ancho imagen) (get-alto imagen) (flipH-rec (get-ancho imagen) (get-pixel imagen) set-valor))]
    [(bitmap? imagen) (list (get-ancho imagen) (get-alto imagen) (flipH-rec (get-ancho imagen) (get-pixel imagen) set-valor))]
    [(hexmap? imagen) (list (get-ancho imagne) (get-alto imagen) (flipH-rec (get-ancho imagen) (get-pixel image) set-valor))]
    [else #f]))

(define (flipV-rec alto pixel modypixel)
  (if (null? (cdr pixel))
      (list (modypixel [car pixelList] [- height (cadar pixelList)]))
      (append (list (modypixel [car pixelList] [- height (cadar pixelList)])) (flipV-rec height (cdr pixelList) modypixel))))
;;MODIFICADORES(SET)
;Modificaodr que permite invertir una imágen verticalmente
;Dominio: imagen
;Recorrido: imagen
(define (flipV image)
  (cond
    [(pixmap? imagen) (list (get-ancho imagen) (get-alto imagen) (flipV-rec (get-alto imagen) (get-pixel imagen) set-valor))]
    [(bitmap? imagen) (list (get-ancho imagen) (get-alto imagen) (flipV-rec (get-alto imagen) (get-pixel imagen) set-valor))]
    [(hexmap? imagen) (list (get-ancho imagen) (get-alto imagen) (flipV-rec (get-alto imagen) (get-pixel imagen) set-valor))]
    [else #f]))

;Modificaodor del ancho de la imagen
;Dominio: Lista
;Recorrido: Lista
(define (set-ancho mi-imagen new-ancho)
  (imagen( get-largo mi-imagen)
         new-ancho
         (get-pixel mi-imagen)))

