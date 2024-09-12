#lang racket

;   >          <                 (cambiar-nodo '((0 0 0) (0 0 0) (0 0 0) (0 0 0)) 3 1 1)

(provide (all-defined-out))

(define (crear-lista num x)
  (cond ((<= num 0 ) '())
        (else (cons x (crear-lista (- num 1) x)))))

(define (get-row row matriz)
  (list-ref matriz row))

(define (get-col col matriz)
  (cond ((null? matriz) '())
        (else (cons (list-ref (car matriz) col)
                    (get-col col (cdr matriz))))))

(define (greedy matriz player enemy) (print 3))

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