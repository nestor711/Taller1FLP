#lang eopl

(define (prod-scalar-matriz mat vec)
  (define (producto_escalable row vec)
    (cond
      ((null? row) '()) ; Si la fila es vacía, el resultado es una lista vacía.
      (else (cons (* (car row) (car vec)) (producto_escalable (cdr row) (cdr vec))))))

  (define (multiplicar mat vec)
    (cond
      ((null? mat) '()) ; Si la matriz está vacía, el resultado es una lista vacía.
      (else (cons (producto_escalable (car mat) vec) (multiplicar (cdr mat) vec)))))

  (multiplicar mat vec)
)