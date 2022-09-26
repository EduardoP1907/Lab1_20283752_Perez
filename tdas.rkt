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

(define (imagen? imagen)
  (if (or (bitmap? imagen) (pixmap? imagen) (hexmap? imagen))#t
      #f))


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


;;Modificador que permite recortar una imágen a partir de un cuadrante(Crop)
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

;;Modificador que permite transformar una imagen desde una representación RGB a una representación HEX(INT to Char)
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







;;AQUI VA UN HISTOGRAMA












;;Modificador que permite rotar la imágen 90° a la derecha.(ROTATE 90)
;Dominio: imagen
;Recorrido: imagen

(define (flipXYpixel pixel)
  (cond
    [(pixbit? pixel) (list (get-yBIT pixel) (get-xBIT pixel) (get-bit pixel) (get-depth pixel))]
    [(pixhex? pixel) (list (get-yHEX pixel) (get-xHEX pixel) (get-hex pixel) (get-dHEX pixel))]
    [(pixrgb? pixel) (list (get-yRGB pixel) (get-xRGB pixel) (get-r) (get-g pixel) (get-b pixel) (get-dRGB pixel))]
    [else #f]))

(define (flipXYimage imagen)
  (list (get-largo imagen) (get-ancho imagen) (map flipXYpixel (get-pixel imagen))))

(define (rotate90 imagen)
  (if (imagen? imagen)
      (flipH (flipXYimage imagen))
      #f))

;;Modificador que permite aplicar funciones especiales a las imágenes. Por ejemplo, para modificar colores en alguno de los canales, pasar a blanco y negro, etc.(EDIT)
;Dominio: f X imagen
;recorrido: imagen
(define (edit f imagen)
  (if (imagen? imagen)
      (list (get-ancho imagen) (get-largo) (map f (get-pixel imagen)))
      #f))

;;Modificador que permite obtener el valor del bit opuesto.
;Dominio: pixbit-d
;Recorrido: pixbit-d

(define (invertColorBit pixbit-d)
  (if (pixbit? pixbit-d)
      (cond
        [(= 1 (get-bit pixbit-d)) (set-valorBIT pixbit-d 0)]
        [(= 0 (get-bit pixbit-d)) (set-valorBIT pixbit-d 1)])
      pixbit-d))

;;Modificasdor que permite obtener el color simétricamente opuesto en cada canal dentro de un pixel.
;Dominio: pixrgb-d
;Recorrido: pixrgb-d

(define (invertColorRGB pixrgb-d)
  (if(pixrgb? pixrgb-d) #t
      (list (get-xRGB pixrgb-d) (get-yRGB pixrgb-d) [- 255 (get-r pixrgb-d)] [- 255 (get-g pixrgb-d)] [- 255 (get-b pixrgb-d)] (get-dRGB pixrgb-d))))
