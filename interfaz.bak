#lang racket/gui
(require racket/gui/base)

; Crear la ventana de inicio y bienvenida
(define Ventana1
  (new frame%
       [label "Inicio"]
       [width 700]
       [height 700]))


; Crear una caja para guardar la etiqueta de bienvenida
(define box1
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para guardar la etiqueta de tamaño de matriz
(define box2
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para las entradas de texto
(define box3
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para las entradas de texto
(define box4
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

; Crear una caja para mostrar mensaje de error de entrada
(define box5
  (new horizontal-panel%
       [parent Ventana1]
       [alignment '(center top)]  ; Centrar en la parte de arriba
       [stretchable-height #f]))

;"""""""""""""""""""""""""Mostrar mensajes en pantalla""""""""""""""""""""""""""""""

; Mostrar mensaje de Bienvenida
(define msg-area (new message%
                      [parent box1]
                      [vert-margin 100]
                      [label "  Bienvenido a Tic Tac Toe"]
                      [min-width 250]
                      [font (make-object font% 25.0 'system)]
                      [auto-resize #t]))

; Mostrar mensaje de elegir tamaño de matriz
(define msg-area2 (new message%
                      [parent box2]
                      [vert-margin 30]
                      [label "  Por favor ingrese el tamaño del tablero"]
                      [min-width 250]
                      [font (make-object font% 18.0 'system)]
                      [auto-resize #t]))

(send Ventana1 show #t)