#lang racket



;----------------------------## Función que busca la solución general ##----------------------------

; Función que recorre cada fila de la matriz y devuelve si hay un ganador en alguna fila
; o en alguna columna.
(define (recorrer-filas matrix)
  ; Verifica si hay un ganador en alguna fila
  (define (check-rows matrix)
    (cond ((null? matrix) #f) ; Si no hay más filas, no se ha encontrado un ganador
          ((buscar-ganador-fila (car matrix)) ; Recorre la primera fila
           #t) ; Si se encuentra un ganador en la primera fila, devuelve #t
          (else
           (check-rows (cdr matrix))))) ; Recurre a las siguientes filas

  ; Verifica si hay un ganador en alguna columna
  (define (check-columns matrix)
    (let ([columns (buscar_ganador_columna matrix)])
      (define (check-all-columns cols)
        (if (null? cols)
            #f ; Si no hay más columnas, no se ha encontrado un ganador
            (if (buscar-ganador-fila (car cols))
                #t ; Si se encuentra un ganador en la columna actual, devuelve #t
                (check-all-columns (cdr cols))))) ; Recurre a las siguientes columnas
      (check-all-columns columns)))

  ; Verifica filas y columnas
  (or (check-rows matrix) (check-columns matrix)))

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



;---------------------------------------------Pruebas-----------------------------------------------
; Definición de la matriz de prueba
(define test-matrix '((1 0 1)
                      (0 0 1)
                      (1 0 1)))

; Ejecución de la prueba
(display "Resultado de recorrer-filas para test-matrix: ")
(display (recorrer-filas test-matrix))
(newline)

; Definición de una matriz sin ganador
(define no-winner-matrix '((0 1 0)
                           (1 0 1)
                           (0 1 0)))

; Ejecución de la prueba sin ganador
(display "Resultado de recorrer-filas para no-winner-matrix: ")
(display (recorrer-filas no-winner-matrix))
(newline)
