#lang eopl

(define (make-tree value left right)
  (list value left right))

(define (value tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (empty? tree)
  (null? tree))

(define (path n BST)
  (cond
    ((empty? BST) '()) ;; Árbol vacío, no se encuentra n
    ((= n (value BST)) '()) ;; Valor encontrado en la raíz, lista vacía
    ((< n (value BST)) ;; Valor menor que el nodo actual, ir a la izquierda
     (cons 'left (path n (left BST))))
    (else ;; Valor mayor que el nodo actual, ir a la derecha
     (cons 'right (path n (right BST))))
    )
  )