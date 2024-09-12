#lang racket

;   >          <                 (cambiar-nodo '((0 0 0) (0 0 0) (0 0 0) (0 0 0)) 3 1 1)

(provide (all-defined-out))

(define (crear-lista num x)
  (cond ((<= num 0 ) '())
        (else (cons x (crear-lista (- num 1) x)))))

(define (matriz n m)
  (cond ((equal? m 0) "Elige un tamaño de columna válido")
        ((equal? n 0) "Elige un tamaño de fila válido")
        (else (append (crear-lista n (crear-lista m 0))))))

(define (print-matriz matriz)
  (cond
    ((null? matriz) (newline))
    (else (begin
            (display (car matriz))
            (newline)
            (print-matriz (cdr matriz))))))

; Cambia un nodo de la lista
(define (cambiar-nodo-lista lst m elem)  ;Secundaria para manejar primero las filas
    (cond ((equal? m 0) (cons elem (cdr lst)))   
          (else
           (cons (car lst) (cambiar-nodo-lista (cdr lst) (- m 1) elem)))))
;Cambia un nodo en las columnas 
(define (cambiar-nodo matriz n m elem)
  (cond ((equal? n 0) (cons (cambiar-nodo-lista (car matriz) m elem) (cdr matriz))) ; Columnas
        (else
         (cons (car matriz) (cambiar-nodo (cdr matriz) (- n 1) m elem))))) 



(define (a)
  (print-matriz (matriz 3 4)))



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
      (if (get_winner_as_row (car cols))
          #t ; Si se encuentra un ganador en la columna actual, devuelve #t
          (check-all-columns (cdr cols))))) ; Recurre a las siguientes columnas


; Función que recorre cada fila de la matriz y devuelve si hay un ganador en alguna fila
(define (check-rows matrix)
  (cond ((null? matrix) #f) ; Si no hay más filas, no se ha encontrado un ganador
        ((get_winner_as_row (car matrix)) ; Recorre la primera fila
         #t) ; Si se encuentra un ganador en la primera fila, devuelve #t
        (else
         (check-rows (cdr matrix))))) ; Recurre a las siguientes filas

; Función que verifica si hay un ganador en alguna diagonal
(define (check-diagonals matrix)
  (let ((descendants (descending_diagonals matrix))
        (ascending (ascending_diagonals matrix)))
    (or (check-all-columns descendants) ; Verifica diagonales descendentes
        (check-all-columns ascending)))) ; Verifica diagonales ascendentes



;----------------------------## Función que busca la solución como una fila ##----------------------------

; Función que busca un ganador en una fila y devuelve #t o #f
(define (get_winner_as_row fila)
  
  (display "Analyzing row: ") (display fila) (newline)
  
  (if (null? fila)
      #f
      (search_secuence (cdr fila) (car fila) 1))) ; Llama a search_secuence con el primer elemento como referencia

;; Función auxiliar que busca una secuencia ganadora en una fila
(define (search_secuence fila ele counter)
  (cond
    ;; Caso base: si se recorren todos los elementos, verifica si el contador es al menos 3
    ((null? fila) (>= counter 3)) ; Si se llega al final y el contador es 3 o más, devuelve #t
    ;; Si el elemento actual es 0, reinicia el contador a 0 y continúa
    ((zero? (car fila)) (search_secuence (cdr fila) ele 0))
    ;; Si el elemento actual es igual al elemento a buscar
    ((= (car fila) ele)
     (if (>= (+ counter 1) 3)
         #t ; Si se encuentra una secuencia de 3 o más, devuelve #t
         (search_secuence (cdr fila) ele (+ counter 1)))) ; De lo contrario, continúa buscando
    ;; Si el elemento actual es diferente al elemento a buscar, reinicia el contador a 1
    (else (search_secuence (cdr fila) (car fila) 1))))



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

;; Function to obtain diagonals from the top-left corner to the bottom-right corner.
(define (descending_diagonals matrix)
  ;; Helper function to extract a diagonal starting from a specific position.
  (define (extract_diagonal matrix start-row start-col)
    ;; Recursive function to extract elements from the diagonal.
    (define (extract row col acc)
      (cond
        ;; Caso base: si la fila o la columna están fuera de los límites de la matriz, retornar la lista acumulada en orden inverso.
        ((or (>= row (length matrix)) (>= col (length (car matrix)))) (reverse acc))
        ;; Caso recursivo: agregar el elemento actual a la acumulación y mover a la siguiente posición diagonal.
        (else (extract (+ row 1) (+ col 1) (cons (list-ref (list-ref matrix row) col) acc)))))
    ;; Llamar a la función recursiva comenzando desde la fila y columna iniciales con una lista acumulativa vacía.
    (extract start-row start-col '()))
  
  ;; Function to get all diagonals starting from a specific row.
  (define (diagonals-from-row matrix row)
    (if (>= row (length matrix))
        ;; Caso base: si la fila está fuera de los límites (mayor o igual que el número de filas), retornar una lista vacía.
        '()
        ;; Caso recursivo: obtener la diagonal desde la fila actual y continuar con la siguiente fila.
        (cons (extract_diagonal matrix row 0)
              (diagonals-from-row matrix (+ row 1)))))
  
  ;; Function to get all diagonals starting from a specific column.
  (define (diagonals-from-column matrix column)
    (if (>= column (length (car matrix)))
        ;; Caso base: si la columna está fuera de los límites (mayor o igual que el número de columnas), retornar una lista vacía.
        '()
        ;; Caso recursivo: obtener la diagonal desde la columna actual y continuar con la siguiente columna.
        (cons (extract_diagonal matrix 0 column)
              (diagonals-from-column matrix (+ column 1)))))
  
  ;; Append the diagonals obtained from rows and columns, starting from the first row and the second column.
  (append (diagonals-from-row matrix 0)
          (diagonals-from-column matrix 1)))


;----------------------------## Función que obtiene las diagonales de esquina superior derecha a esquina inferior izquierda ##----------------------------

;; The main function to obtain ascending diagonals of a matrix.
(define (ascending_diagonals matrix)
  ;; Función auxiliar para extraer una diagonal ascendente comenzando desde una posición específica.
  (define (extract_diagonal matrix start-row start-col)
    ;; Función recursiva para extraer los elementos de la diagonal ascendente.
    (define (extract row col acc)
      (cond
        ;; Caso base: si la fila está fuera de los límites (menor que 0), retornar la lista acumulada en orden inverso.
        ((< row 0) (reverse acc))
        ;; Caso base: si la columna está fuera de los límites (mayor o igual que el número de columnas), retornar la lista acumulada en orden inverso.
        ((>= col (length (car matrix))) (reverse acc))
        ;; Caso recursivo: agregar el elemento actual a la acumulación y mover a la siguiente posición diagonal.
        (else (extract (- row 1) (+ col 1) (cons (list-ref (list-ref matrix row) col) acc)))))
    ;; Llamar a la función recursiva comenzando desde la fila y columna iniciales con una lista acumulativa vacía.
    (extract start-row start-col '()))
  
  ;; Función para obtener todas las diagonales ascendentes comenzando desde una fila específica.
  (define (diagonals-from-row matrix row)
    (if (< row 0)
        ;; Caso base: si la fila está fuera de los límites (menor que 0), retornar una lista vacía.
        '()
        ;; Caso recursivo: obtener la diagonal desde la fila actual y continuar con la fila anterior.
        (cons (extract_diagonal matrix row 0)
              (diagonals-from-row matrix (- row 1)))))
  
  ;; Función para obtener todas las diagonales ascendentes comenzando desde una columna específica.
  (define (diagonals-from-column matrix column)
    (if (>= column (length (car matrix)))
        ;; Caso base: si la columna está fuera de los límites (mayor o igual que el número de columnas), retornar una lista vacía.
        '()
        ;; Caso recursivo: obtener la diagonal desde la columna actual y continuar con la siguiente columna.
        (cons (extract_diagonal matrix (sub1 (length matrix)) column)
              (diagonals-from-column matrix (+ column 1)))))
  
  ;; Unir las diagonales obtenidas desde las filas y columnas, comenzando desde la última fila y la primera columna.
  (append (diagonals-from-row matrix (- (length matrix) 1))
          (diagonals-from-column matrix 0)))

;---------------------------------------------Pruebas para el algoritmo codicioso---------------------------------------------

;----------------------------## Función para encontrar el mejor movimiento ##----------------------------

; Encuentra el mejor movimiento utilizando un algoritmo codicioso
; Encuentra el mejor movimiento utilizando un algoritmo codicioso
(define (best-move matrix player)
  ; Encuentra todas las posiciones vacías en la matriz
  (define empty-positions (find-empty-positions matrix))
  
  ; Evalúa el puntaje de un movimiento dado
  (define (evaluate-move move)
    (define new-matrix (place-move matrix move player))
    (if (get_solution new-matrix)
        ; Si el jugador es 1 (X), queremos ganar, asigna un puntaje alto
        (if (= player 1)
            10
            ; Si el jugador es 2 (O), queremos minimizar el puntaje, asigna un puntaje bajo
            -10)
        ; Si no hay ganador, el puntaje es 0
        0))
  
  ; Encuentra el movimiento con el mayor puntaje
  (define (find-best-move positions)
    (cond
      ((null? positions) #f)
      (else
       (define move (car positions))
       (define rest (cdr positions))
       (define best (find-best-move rest))
       (if (or (not best)
               (> (evaluate-move move) (evaluate-move best)))
           move
           best))))
  
  ; Llama a la función para encontrar el mejor movimiento entre todas las posiciones vacías
  (find-best-move empty-positions))

; Encuentra todas las posiciones vacías en la matriz
(define (find-empty-positions matrix)
  (define (find-positions matrix row col acc)
    (cond
      ((>= row (length matrix)) (reverse acc))
      ((>= col (length (car matrix))) (find-positions matrix (+ row 1) 0 acc))
      ((= (list-ref (list-ref matrix row) col) 0)
       (find-positions matrix row (+ col 1) (cons (list row col) acc)))
      (else (find-positions matrix row (+ col 1) acc))))
  (find-positions matrix 0 0 '()))

; Coloca un movimiento en la matriz
(define (place-move matrix move player)
  ; Reemplaza el valor en la posición especificada con el valor del jugador
  (define (replace-at matrix row col value)
    (list-set matrix row
              (list-set (list-ref matrix row) col value)))
  
  ; Llama a la función para reemplazar el valor en la matriz en la posición del movimiento
  (replace-at matrix (first move) (second move) player))


;---------------------------------------------Pruebas-----------------------------------------------
; Ejemplo de uso con el jugador 1 (X)
(define test-matrix
  '((1 0 0)
    (0 0 2)
    (1 0 1)))

(display "Mejor movimiento para jugador 1 (X): ")
(display (best-move test-matrix 1))
(newline)

; Ejecución de la prueba
;(display "Resultado de recorrer-filas para test-matrix: ")
;(display (get_solution test-matrix))
;(newline)
