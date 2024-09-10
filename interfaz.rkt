#lang racket/gui
(require racket/gui/base)

; Crear la ventana de inicio y bienvenida
(define Menu
  (new frame%
       [label "Inicio"]
       [width 600]
       [height 600]))



; Crear una caja para guardar la etiqueta de bienvenida
(define box1
  (new horizontal-panel%
       [parent Menu]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para guardar la etiqueta de tamaño de matriz
(define box2
  (new horizontal-panel%
       [parent Menu]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para las entradas de texto
(define box3
  (new horizontal-panel%
       [parent Menu]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para las entradas de texto
(define box4
  (new horizontal-panel%
       [parent Menu]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para mostrar mensaje de error de entrada
(define box5
  (new horizontal-panel%
       [parent Menu]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

;"""""""""""""""""""""""""Mostrar mensajes en pantalla""""""""""""""""""""""""""""""

; Mostrar mensaje de Bienvenida
(define msg-area (new message%
                      [parent box1]
                      [vert-margin 100]
                      [label "  Has iniciado el juego Tic Tac Toe"]
                      [min-width 250]
                      [font (make-object font% 25.0 'system)]
                      [auto-resize #t]))

; Mostrar mensaje de elegir tamaño de matriz
(define msg-area2 (new message%
                      [parent box2]
                      [vert-margin 30]
                      [label "  Ingrese el tamaño del tablero"]
                      [min-width 250]
                      [font (make-object font% 18.0 'system)]
                      [auto-resize #t]))

(define entradaColumnas (new text-field%
                       [label "Columnas"]
                       [parent box3]
                       [min-width 100]  ; Ancho fijo para la caja de texto
                       [min-height 30]  ; Altura fija para la caja de texto
                       [font (make-object font% 14.0 'system)])) ; Tamaño del texto

(define entradaFilas (new text-field%
                       [label "     Filas"]
                       [parent box3]
                       [min-width 100]  ; Ancho fijo para la caja de texto
                       [min-height 30]  ; Altura fija para la caja de texto
                       [font (make-object font% 14.0 'system)])) ; Tamaño del texto

; Definir variables globales para los mensajes de error
(define mensaje-error-columnas #f)
(define mensaje-error-filas #f)

; Crear dos cajas separadas para los mensajes de error
(define box-error-columnas
  (new vertical-panel%
       [parent Menu]
       [alignment '(center top)]
       [stretchable-height #f]))

(define box-error-filas
  (new vertical-panel%
       [parent Menu]
       [alignment '(center top)]
       [stretchable-height #f]))

; Función para actualizar o crear un mensaje de error
(define (actualizar-mensaje-error box mensaje texto)
  (if mensaje
      (begin
        (send mensaje set-label texto)
        (send mensaje show #t))
      (set! mensaje (new message% 
                         [parent box] 
                         [label texto] 
                         [font (make-object font% 14.0 'system)])))
  mensaje)

; Función que se llama cuando se presiona el botón
(define (llamadaBoton button event)
  (let ((numColumnas (string->number (send entradaColumnas get-value)))
        (numFilas (string->number (send entradaFilas get-value))))
    
    ; Actualizar mensaje de error para columnas
    (set! mensaje-error-columnas
      (cond
        [(or (not numColumnas) (< numColumnas 3))
         (actualizar-mensaje-error box-error-columnas mensaje-error-columnas 
                                   "No se puede crear un tablero de menos de 3 columnas")]
        [(> numColumnas 10)
         (actualizar-mensaje-error box-error-columnas mensaje-error-columnas 
                                   "No se puede crear un tablero de más de 10 columnas")]
        [else 
         (when mensaje-error-columnas
           (send mensaje-error-columnas show #f))
         #f]))
    
    ; Actualizar mensaje de error para filas
    (set! mensaje-error-filas
      (cond
        [(or (not numFilas) (< numFilas 3))
         (actualizar-mensaje-error box-error-filas mensaje-error-filas 
                                   "No se puede crear un tablero de menos de 3 filas")]
        [(> numFilas 10)
         (actualizar-mensaje-error box-error-filas mensaje-error-filas 
                                   "No se puede crear un tablero de más de 10 filas")]
        [else 
         (when mensaje-error-filas
           (send mensaje-error-filas show #f))
         #f]))
    
    ; Si no hay errores, llamar a Cuadricula
    ;(when (and (not mensaje-error-columnas) (not mensaje-error-filas))
      ;(displayln "Llamando a Cuadricula")
      ;(Cuadricula numFilas numColumnas 1 1))
    ))

;"""""""""""""""""""""""""""""""""""Botón para validar datos ingresados""""""""""""""""""""""
;Crea el botón check    
(new button% [label "Validar"]
    [parent box4]
    [vert-margin 50]
    [min-width 100]
    [font (make-object font% 20.0 'system)]
    [callback llamadaBoton])
    
; Centrar la ventana1
(send Menu center)

; Mostrar la ventana
(send Menu show #t)