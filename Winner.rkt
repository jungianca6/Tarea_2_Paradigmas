#lang racket



;----------------------------## Función que busca la solución general ##----------------------------
; Función principal que verifica filas, columnas y diagonales
(define (get_solution matrix)
  (or (check-rows matrix) ; Verifica filas
      (check-columns matrix) ; Verifica columnas
      (check-diagonals matrix))) ; Verifica diagonales


; Función que verifica si hay un ganador en alguna columna
(define (check-columns matrix)
  (check-all-columns (buscar_ganador_columna matrix)))

; Verifica si hay un ganador en alguna columna
(define (check-all-columns cols)
  (if (null? cols)
      #f ; Si no hay más columnas, no se ha encontrado un ganador
      (if (buscar-ganador-fila (car cols))
          #t ; Si se encuentra un ganador en la columna actual, devuelve #t
          (check-all-columns (cdr cols))))) ; Recurre a las siguientes columnas



; Función que recorre cada fila de la matriz y devuelve si hay un ganador en alguna fila
(define (check-rows matrix)
  (cond ((null? matrix) #f) ; Si no hay más filas, no se ha encontrado un ganador
        ((buscar-ganador-fila (car matrix)) ; Recorre la primera fila
         #t) ; Si se encuentra un ganador en la primera fila, devuelve #t
        (else
         (check-rows (cdr matrix))))) ; Recurre a las siguientes filas

; Función que verifica si hay un ganador en alguna diagonal
(define (check-diagonals matrix)
  (let ((descendentes (diagonales-descendentes matrix))
        (ascendentes (diagonales-ascendentes matrix)))
    (or (check-all-columns descendentes) ; Verifica diagonales descendentes
        (check-all-columns ascendentes)))) ; Verifica diagonales ascendentes




;----------------------------## Función que busca la solución como una fila ##----------------------------
; Función auxiliar que busca una secuencia ganadora en una fila
(define (buscar-secuencia fila ele counter)
  (cond
    ((null? fila) (>= counter 3)) ; Si se recorren todos los elementos, verifica el contador
    ((zero? ele) (buscar-secuencia (cdr fila) (car fila) 1)) ; Reinicia el contador si el elemento es cero
    ((= (car fila) ele)
     (buscar-secuencia (cdr fila) ele (+ counter 1))) ; Aumenta el contador si el elemento es igual
    (else
     (buscar-secuencia (cdr fila) (car fila) 1)))) ; Reinicia el contador si el elemento no es igual

; Función que busca un ganador en una fila y devuelve #t o #f
(define (buscar-ganador-fila fila)
  (if (null? fila)
      #f
      (buscar-secuencia (cdr fila) (car fila) 1))) ; Llama a buscar-secuencia con el primer elemento como referencia

;----------------------------## Función que convierte las columnas en filas para buscar las soluciones ##----------------------------
; Función que obtiene los valores de una columna específica en una matriz.
(define (get-column matrix col-index)
  ; Función auxiliar que recorre las filas de la matriz y extrae
  ; el valor de la columna especificada.
  (define (extract-column rows col-index acc)
    (if (null? rows)
        (reverse acc) ; Devuelve la lista acumulada en el orden correcto
        (extract-column (cdr rows) ; Continúa con las filas restantes
                        col-index ; Mantiene el índice de columna
                        (cons (list-ref (car rows) col-index) acc)))) ; Extrae el valor de la columna y lo agrega al acumulador
  ; Llama a la función auxiliar con una lista vacía como acumulador inicial
  (extract-column matrix col-index '()))

; Función que obtiene todas las columnas de una matriz
(define (buscar_ganador_columna matrix)
  ; Determina el número de columnas basado en la primera fila de la matriz
  (define (column-count matrix)
    (length (car matrix)))
  
  ; Función auxiliar que recorre todos los índices de columna
  ; y llama a `get-column` para obtener los valores de cada columna.
  (define (extract-all-columns matrix col-index acc)
    (if (>= col-index (column-count matrix))
        (reverse acc) ; Devuelve la lista de columnas acumuladas en el orden correcto
        (extract-all-columns matrix
                             (+ col-index 1) ; Avanza al siguiente índice de columna
                             (cons (get-column matrix col-index) acc)))) ; Extrae la columna y la agrega al acumulador

  ; Llama a la función auxiliar con el índice de columna inicial 0
  (extract-all-columns matrix 0 '()))

;----------------------------## Funciones para obtener las diagonales ##----------------------------

; Función que obtiene las diagonales de esquina superior izquierda a esquina inferior derecha
(define (diagonales-descendentes matrix)
  (define (extraer-diagonal matrix start-row start-col)
    (define (extraer fila col acc)
      (cond
        ((or (>= fila (length matrix)) (>= col (length (car matrix)))) (reverse acc))
        (else (extraer (+ fila 1) (+ col 1) (cons (list-ref (list-ref matrix fila) col) acc)))))
    (extraer start-row start-col '()))
  
  (define (diagonales-desde-fila matrix fila)
    (if (>= fila (length matrix))
        '()
        (cons (extraer-diagonal matrix fila 0)
              (diagonales-desde-fila matrix (+ fila 1)))))
  
  (define (diagonales-desde-columna matrix columna)
    (if (>= columna (length (car matrix)))
        '()
        (cons (extraer-diagonal matrix 0 columna)
              (diagonales-desde-columna matrix (+ columna 1)))))
  
  (append (diagonales-desde-fila matrix 0)
          (diagonales-desde-columna matrix 1)))


;----------------------------## Función que obtiene las diagonales de esquina superior derecha a esquina inferior izquierda ##----------------------------

(define (diagonales-ascendentes matrix)
  (define (extraer-diagonal matrix start-row start-col)
    (define (extraer fila col acc)
      (cond
        ((< fila 0) (reverse acc))
        ((>= col (length (car matrix))) (reverse acc))
        (else (extraer (- fila 1) (+ col 1) (cons (list-ref (list-ref matrix fila) col) acc)))))
    (extraer start-row start-col '()))
  
  (define (diagonales-desde-fila matrix fila)
    (if (< fila 0)
        '()
        (cons (extraer-diagonal matrix fila 0)
              (diagonales-desde-fila matrix (- fila 1)))))
  
  (define (diagonales-desde-columna matrix columna)
    (if (>= columna (length (car matrix)))
        '()
        (cons (extraer-diagonal matrix (sub1 (length matrix)) columna)
              (diagonales-desde-columna matrix (+ columna 1)))))
  
  (append (diagonales-desde-fila matrix (- (length matrix) 1))
          (diagonales-desde-columna matrix 0)))


;---------------------------------------------Pruebas-----------------------------------------------
; Definición de la matriz de prueba
(define test-matrix '((1 1 0 1 0 1)
                      (0 0 0 0 1 0)
                      (0 0 1 1 0 0)
                      (0 0 0 0 0 0)
                      (0 0 1 0 1 0)
                      (0 0 1 1 0 0)))

; Ejecución de la prueba
(display "Resultado de recorrer-filas para test-matrix: ")
(display (get_solution test-matrix))
(newline)

; Definición de una matriz sin ganador
(define no-winner-matrix '((0 1 0)
                           (1 0 1)
                           (0 1 0)))

; Ejecución de la prueba sin ganador
(display "Resultado de recorrer-filas para no-winner-matrix: ")
(display (get_solution no-winner-matrix))
(newline)
