#lang eopl

;; Ejercicio 1
;; Función invert :
;; Proposito:
;; invert L -> la función recibe una lista, debe retornar la misma lista con sus elementos
;;             invertidos.
;;
;; Gramática BNF
;; <list_par> ::= '() | (<par> <lista_par>) | <par>
;; <par> ::= (<elemento 1> <elemento 2>)
;; <elemento 1> ::= <lista>
;; <elemento 2> ::= <lista>
;; <lista> ::= <elemento> | <elemento> <lista>
;; <elemento> ::= <int> | <symbol> 

(define invert
  (lambda (l) ;recibe una lista
    (cond ((null? l) '()) ;si la lista es vacía retorna una lista vacía
        (else (cons (list (cadr (car l)) (car (car l))) ;se construye una nueva lista con el primer par de la lista, se toma el resto del primer par como primer elemento y el segundo elemento será la cabeza del primer par.
                    (invert (cdr l))))))); el resto de los pares se envía de forma recursiva para analizar los isguientes pares de la lista


;; Pruebas
;; >  (invert '((a 1) (a 2) (1 b) (2 b)))
;;    ((1 a) (2 a) (b 1) (b 2))

;; > (invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
;;   ((9 5) (91 10) (7 82) (e a) ("Mundo" "hola"))

;; >  (invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))
;;    (("racket" "es") ("muy" "genial") (29 17) (o 81))
