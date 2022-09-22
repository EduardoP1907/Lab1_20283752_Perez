#lang racket


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

(define (image?-specific-wrapper ancho largo pixelList evaluator)
  (if (null? pixelList)
      #t
      (if (and [evaluator (car pixelList)] [>= ancho (car (car pixelList))] [>= largo (car (cdr (car pixelList)))])
          (image?-specific-wrapper ancho largo (cdr pixelList) evaluator)
          #f)))

(define (image?-specific image evaluator)
  (if (and (positive? (get-ancho image)) (positive? (get-largo imagen)))
      (image?-specific-wrapper (get-ancho image) (get-largo imagen) (get-pixel imagen) evaluator)
      #f))

(define (bitmap? imagen)
  (if (imagen?-specific imagen pixbit-d?)
      #t
      #f))

(define (pixmap? imagen)
  (if (imagen?-specific imagen pixrgb-d?)
      #t
      #f))

(define (hexmap? imagen)
  (if (imagen?-specific imagen pixhex-d?)
      #t
      #f))

(define (imagen? imagen)
  (if (or (bitmap? imagen) (pixmap? imagen) (hexmap? imagen))
      #t
      #f))


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

