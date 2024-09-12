#lang racket/gui

(require racket/gui/base)
(require "Logica_interna.rkt")

; Crear la ventana de inicio y bienvenida
(define Menu
  (new frame%
       [label "Inicio"]
       [width 1000]
       [height 800]))

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
    (when (and (not mensaje-error-columnas) (not mensaje-error-filas))
      (displayln "Llamando a Cuadricula")
      (set! tabla (matriz numFilas numColumnas))
      (create-board-panel tabla)
      (send Menu show #f)
      (send Matriz show #t))))


;"""""""""""""""""""""""""""""""""""Botón para validar datos ingresados""""""""""""""""""""""
;Crea el botón check    
(new button% [label "Validar"]
    [parent box4]
    [vert-margin 50]
    [min-width 100]
    [font (make-object font% 20.0 'system)]
    [callback llamadaBoton])
 
;""""""""""""""""""""""""""""""""""""""""""""""""Matriz""""""""""""""""""""""""""""""""""""""
(define tabla '())
(define actual-player 1)
(define buttons-panel '())

;; Cambia el juagador actual el otro
(define (change-player p)
  (cond[(equal? 1 p) (set! actual-player 2)]
       [else (set! actual-player 1)]))

;; Crea frame 2 del Juego
(define Matriz (new frame%
                    [label "Tic-Tac-Toe"]
                    [width 100]
                    [height 100]))

;; Crea una caja para guardar el panel de botones
(define box6 
  (new horizontal-panel%
       [parent Matriz]
       [alignment '(center top)]))


;; Botones de la matrix de botones
(define Button%
  (class button%
    (init-field [row 0] [col 0])
    (super-new)
    (define/public (get-row) row)
    (define/public (get-col) col)))

; Panel de la matriz
(define (create-board-panel board) 
  (define panel (new vertical-panel%
                     [parent box6]
                     [alignment '(center top)]
                     [stretchable-width #f]))
  
  (define (create-row-panel i)   ; Cada fila
    (define row-panel (new horizontal-panel%
                           [parent panel]
                           [alignment '(center top)]
                           [stretchable-width #f]))
    
    (define row-buttons '())
    (define (create-button j)    ; Crear un botón dentro de la fila
      (define b (new Button% [parent row-panel]
                            [label " "]
                            [callback button-grid-callback]
                            [vert-margin 10]
                            [horiz-margin 10]
                            [min-width 50]
                            [min-height 50]
                            [row i]
                            [col j]))
      ; Añadir el botón a la lista de la fila
      (set! row-buttons (append row-buttons (list b)))  
      (cond ((< j (- (length (list-ref board i)) 1))
             (create-button (+ j 1)))))
    (create-button 0)
    ; Añadir la fila a la matriz de botones
    (set! buttons-panel (append buttons-panel (list row-buttons)))
    (cond     ; Crear la siguiente fila si es necesario
    ((< i (- (length board) 1))
           (create-row-panel (+ i 1)))))
  (create-row-panel 0)   ; Crear la primera fila
  panel)


;; Funcion que se ejecuta cuando un boton se presiona
(define (button-grid-callback b e)
  (set! tabla (cambiar-nodo tabla (send b get-row) (send b get-col) actual-player))

  (displayln (format "Jugador ~a presiono boton (~a, ~a)" 
                                      actual-player
                                      (send b get-row) 
                                      (send b get-col)))
  (print-matriz tabla)
  (update-board-panel)
  (print (send b get-row))
  (print (send b get-col))
  (cond
    (
  ;; Juega Greddy
     (change-player actual-player)
     (define play-greddy
       (cond 
         ((equal? actual-player 2)
          (greedy tabla actual-player 1))
         (else
          (greedy tabla actual-player 2))))
     (set! tabla (cambiar-nodo tabla (send b get-row) (send b get-col) 1))
  
     (displayln (format "El Goloso usa ficha ~a puso en col ~a" 
                        actual-player
                        play-greddy))
     (print-matriz tabla)
     (update-board-panel)
     ;; Sede turno player
     (change-player actual-player)
     )
    )
  )
  
;; Actualiza como se ve la matriz de botones
(define (update-board-panel)
  (define (update-board-panel-helper i j)
    (cond
      [(>= i (length tabla)) #f] ; Caso base: Si i es mayor que la longitud de la lista, termina la recursión
      [(>= j (length (list-ref tabla i))) ; Si j es mayor que la longitud de la sublista i-ésima, pasa a la siguiente sublista
        (update-board-panel-helper (+ i 1) 0)]
      [else ; Si no, actualiza el botón correspondiente y pasa al siguiente elemento de la sublista
        (define button (list-ref (list-ref buttons-panel i) j))
        (cond
          [(equal? 0 (list-ref (list-ref tabla i) j))
            (send button set-label "-")
            (send button enable #t)]
          [(equal? 1 (list-ref (list-ref tabla i) j))
            (send button set-label "1")
            (send button enable #f)]
          [(equal? 2 (list-ref (list-ref tabla i) j))
            (send button set-label "2")
            (send button enable #f)]
        )
        (update-board-panel-helper i (+ j 1))
      ]
    )
  )
  (update-board-panel-helper 0 0)
)


; Centrar la ventana1
(send Menu center)
(send Matriz center)

; Mostrar la ventana
(send Menu show #t)