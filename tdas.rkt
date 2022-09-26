#lang racket
(require "tdaPixbit.rkt")
(require "tdaPixrgb.rkt")
(require "tdaPixhex.rkt")
(require "FuncionesEscenciales.rkt")
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
    [(pixmap? imagen) (list (get-ancho imagen) (get-largo imagen) (flipH-rec (get-ancho imagen) (get-pixel imagen) set-valorxRGB))]
    [(bitmap? imagen) (list (get-ancho imagen) (get-largo imagen) (flipH-rec (get-ancho imagen) (get-pixel imagen) set-valorxBIT))]
    [(hexmap? imagen) (list (get-ancho imagen) (get-largo imagen) (flipH-rec (get-ancho imagen) (get-pixel imagen) set-valorxHEX))]
    [else #f]))

(define (flipV-rec alto pixel modypixel)
  (if (null? (cdr pixel))
      (list (modypixel [car pixel] [- alto (cadar pixel)]))
      (append (list (modypixel [car pixel] [- alto (cadar pixel)])) (flipV-rec alto (cdr pixel) modypixel))))
;;MODIFICADORES(SET)
;Modificaodr que permite invertir una imágen verticalmente
;Dominio: imagen
;Recorrido: imagen
(define (flipV image)
  (cond
    [(pixmap? imagen) (list (get-ancho imagen) (get-largo imagen) (flipV-rec (get-largo imagen) (get-pixel imagen) set-valoryRGB))]
    [(bitmap? imagen) (list (get-ancho imagen) (get-largo imagen) (flipV-rec (get-largo imagen) (get-pixel imagen) set-valoryBIT))]
    [(hexmap? imagen) (list (get-ancho imagen) (get-largo imagen) (flipV-rec (get-largo imagen) (get-pixel imagen) set-valoryHEX))]
    [else #f]))

;Modificador del ancho de la imagen
;Dominio: Lista
;Recorrido: Lista
(define (set-ancho mi-imagen new-ancho)
  (imagen( get-largo mi-imagen)
         new-ancho
         (get-pixel mi-imagen)))


;;Modificador que permite recortar una imágen a partir de un cuadrante
;Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
;Recorrido: imagen
(define (RangoPixel? pixel xMin yMin xMax yMax)
  (if (and (rango? xMin (car pixel) xMax) (rango? yMin (cadr pixel) yMax))
      #t
      #f))

(define (crop-rec pixel xMin yMin xMax yMax xMod yMod)
  (if (null? (cdr pixel))
      (if (RangoPixel? (car pixel) xMin yMin xMax yMax)
          (xMod (yMod (car pixel) (- (caar pixel) xMin)) (- (cadar pixel) yMin))
          '() )
      (if (RangoPixel? (car pixel) xMin yMin xMax yMax)
          (list (xMod (yMod (car pixel) (- (caar pixel) xMin)) (- (cadar pixel) yMin)) (crop-rec (cdr pixel) xMin yMin xMax yMax xMod yMod))
          (crop-rec (cdr pixel) xMin yMin xMax yMax xMod yMod))))

(define (crop imagen x1 y1 x2 y2)
  (if (and [rango? 0 x1 (get-ancho imagen)] [rango? 0 x2 (get-ancho imagen)] [rango? 0 y1 (get-largo imagen)] [rango? 0 y2 (get-largo imagen)])
      (cond
        [(pixmap? imagen) (list (get-ancho imagen) (get-largo imagen) (filter pair? (crop-rec (get-pixel imagen) (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2) set-valorxRGB set-valoryRGB)))]
        [(bitmap? imagen) (list (get-ancho imagen) (get-largo imagen) (filter pair? (crop-rec (get-pixel imagen) (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2) set-valorxBIT set-valoryBIT)))]
        [(hexmap? imagen) (list (get-ancho imagen) (get-largo imagen) (filter pair? (crop-rec (get-pixel imagen) (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2) set-valorxHEX set-valoryHEX)))]
        [else #f])
      #f))

;;Modificador que permite transformar una imagen desde una representación RGB a una representación HEX
;Dominio: imagen 
;Recorrido: imagen

(define (intTocharHex x)
  (let ([letters (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)])
    (if (and (= 0 (remainder x 16)) (= 0 (quotient x 16)))
        '()
        (append (intTocharHex (quotient x 16)) (list (value letters (remainder x 16)))))))

(define (charHexFiller x desiredLength)
  (if (and (not (= (length x) desiredLength)) (> desiredLength (length x)))
      (append '(#\0) (charHexFiller x (- desiredLength 1) ))
      x))

(define (pixelRGB-To-pixelHex p)
  (pixhex-d [set-valorxRGB p] [set-valoryRGB p] (list->string [append (charHexFiller (intTocharHex (set-valorR p)) 2)
                                                                      (charHexFiller (intTocharHex (set-valorG p)) 2)
                                                                      (charHexFiller (intTocharHex (set-valorB p)) 2)]) [set-valorDrgb p]))

(define (pixListRGB-To-pixListHex pList)
  (if (null? (cdr pList))
      (pixelRGB-To-pixelHex (car pList))
      (list (pixelRGB-To-pixelHex (car pList)) (pixelRGB-To-pixelHex (cdr pList)))))
      
(define (imgRGB-To-imgHex imagen)
  (if (pixmap? imagen)
      (list (imagen imagen) (get-ancho imagen) (pixListRGB-To-pixListHex (get-pixel imagen)))
      #f))