#lang eopl


;; Ejercicio 16

;; Gramática BNF
;; <OperacionB>::= <int>
;;             ::= (<OperacionB> ’suma <OperacionB>)
;;             ::= (<OperacionB> ’resta <OperacionB>)
;;             ::= (<OperacionB> ’multiplica <OperacionB>)

;; Operar-binarias: operacionB -> number o 'operacion-no-valida
;; Calcula el resultado de una operación binaria representada como una lista
;; que sigue una gramática específica.
;;
;; operacionB: La operación binaria representada como una lista en la gramática:
;; Un número entero.
;; Una lista de la forma '(operando1 operador operando2)', donde:
;; operando1 y operando2 son operaciones binarias válidas.
;; operador es una de las siguientes palabras clave: 'suma', 'resta' o 'multiplica'.

;; La función devuelve el resultado de la operación binaria o 'operacion-no-valida' si
;; la entrada no sigue la gramática especificada o si la operación no es válida.


(define (Operar-binarias operacionB)
  (cond
    ((number? operacionB) operacionB) ; Si es un número, retornamos el número mismo
    ((pair? operacionB) ; Si es una lista
     (let* ((op (cadr operacionB)) ; Extraemos la operación (suma, resta, multiplica)
            (left (car operacionB)) ; Extraemos el operando izquierdo
            (right (caddr operacionB))) ; Extraemos el operando derecho
       (cond
         ((eq? op 'suma) (+ (Operar-binarias left) (Operar-binarias right))) ; Si la operación es 'suma'
         ((eq? op 'resta) (- (Operar-binarias left) (Operar-binarias right))) ; Si la operación es 'resta'
         ((eq? op 'multiplica) (* (Operar-binarias left) (Operar-binarias right))) ; Si la operación es 'multiplica'
         (else 'operacion-no-valida))) ; En caso de una operación no válida
      )
    (else 'operacion-no-valida))) ; En caso de que no sea un número ni una lista válida


;; Ejemplos de uso
;; (Operar-binarias 4) ; Debe mostrar 4
;; (Operar-binarias '(2 suma 9)) ; Debe mostrar 11
;; (Operar-binarias '(2 resta 9)) ; Debe mostrar -7
;; (Operar-binarias '(2 multiplica 9)) ; Debe mostrar 18
;; (Operar-binarias '((2 multiplica 3) suma (5 resta 1))) ; Debe mostrar 10
;; (Operar-binarias '((2 multiplica (4 suma 1)) multiplica ((2 multiplica 4) resta 1))) ;Debe mostrar 70


