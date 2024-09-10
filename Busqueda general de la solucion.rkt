#lang racket

; '( (0 1 0) (0 1 0) (0 1 0) )
; 0 es igual a una casilla vacia
; 1 es igual a una x
; 2 es igual a una o
; (recorrer-filas '((0 1 0)
;                   (0 1 0)
;                   (0 1 0) ))

;; (recorrer-filas '((0 1 0)(0 1 0) (0 1 0) ))


;----------------------------## Funcion que busca la solucin como una fila ##----------------------------
; Función auxiliar que busca una secuencia ganadora en una fila
(define (buscar-secuencia fila ele counter)
  (cond
    ((null? fila) (>= counter 3)) ; Si se recorren todos los elementos, verifica el contador
    ((zero? ele) (buscar-secuencia (cdr fila) (car fila) 1))
    ((= (car fila) ele)
     (buscar-secuencia (cdr fila) ele (+ counter 1))) ; Aumenta el contador si el elemento es igual
    (else
     (buscar-secuencia (cdr fila) (car fila) 1)))) ; Reinicia el contador si el elemento no es igual

; Función que busca un ganador en una fila y devuelve #t o #f
(define (buscar-ganador-fila fila)
  (if (null? fila)
      #f
      (buscar-secuencia (cdr fila) (car fila) 1))) ; Llama a buscar-secuencia con el primer elemento como referencia



;----------------------------## Funcion que convierte las columnas en filas para buscar las solciones ##----------------------------






;----------------------------## Funcion que busca las diagonales ##----------------------------


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

; Función que obtiene las diagonales de esquina superior derecha a esquina inferior izquierda
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

; Ejemplo de uso para una matriz 4x3
(define matrixx '((1 2 3 4 5)
                 (4 5 6 3 8)
                 (7 8 9 2 3)
                 (10 11 12 11 2)))

(display (diagonales-descendentes matrixx)) ; Diagonales descendentes
(newline)
(display (diagonales-ascendentes matrixx)) ; Diagonales ascendentes







; Función que recorre cada fila de la matriz y devuelve si hay un ganador en alguna fila
(define (recorrer-filas matrix)
  (cond ((null? matrix) #f) ; Si no hay más filas, no se ha encontrado un ganador
        ((buscar-ganador-fila (car matrix)) ; Recorre la primera fila
         #t) ; Si se encuentra un ganador en la primera fila, devuelve #t
        (else
         (recorrer-filas (cdr matrix))))) ; Recurre a las siguientes filas


; Ejemplo de uso para una matriz 3x3
(define matrix '((0 0 0) (0 1 0) (0 1 0)))

(display (recorrer-filas matrix)) ; Esto debería imprimir #t si hay un ganador en alguna fila




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
(define (get-all-columns matrix)
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










