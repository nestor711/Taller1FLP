#lang eopl
;----------INTEGRANTES-------------- 
;
;Kevin Alejandro Velez Agudelo
;kevin.alejandro.velez@correounivalle.edu.co
;
;Moreno Romero Miguel Angel 
;miguel.romero@correounivalle.edu.co
;
;Nestor David Heredia Gutierrez
;nestor.heredia@correounivalle.edu.co




;punto 2

;; down :
;; Proposito:
;; down L -> Lista Función que debe retornar una lista con cada elemento de L asociado a un nivel más de paréntesis comparado con su estado original en L.
;;
;;<list> ::= '() | <element> <list>
;;<element> ::= <atom> | <list>
;;<atom> ::= cualquier átomo (número, símbolo, etc.)

;; down: Procesa una lista anidada y devuelve una nueva lista
;;        donde las sublistas anidadas se representan como listas separadas.
;; Input:
;;   L: La lista que se va a procesar.
;; Output:
;;   Una nueva lista que representa la lista original con sublistas anidadas como listas separadas.
(define (down L)
  (cond
    ;; Caso 1: Si la lista de entrada L es vacía, devuelve una lista vacía.
    ((null? L) '())
    
    ;; Caso 2: Si el primer elemento de la lista L es una lista,
    ;;         llamamos recursivamente a down en el primer elemento y en el resto de la lista.
    ;;         Luego, concatenamos las dos listas resultantes.
    ((list? (car L))
     (cons (down (car L)) (down (cdr L))))
    
    ;; Caso 3: Si el primer elemento de la lista L no es una lista,
    ;;         creamos una nueva lista que contiene ese elemento como su único elemento,
    ;;         y luego llamamos recursivamente a down en el resto de la lista.
    ;;         Luego, concatenamos la lista resultante con el resultado de down en el resto de la lista.
    (else
     (cons (list (car L)) (down (cdr L))))))



;; Pruebas
;; > (down ’(1 2 3))
;;((1) (2) (3))
;;> (down ’((una) (buena) (idea)))
;;(((una)) ((buena)) ((idea)))
;;> (down ’(un (objeto (mas)) complicado))
;;((un) ((objeto (mas))) (complicado))







;punto numero 3

;;primero definimos la funcion...
;; list-set: lista número cualquier -> lista
;; Reemplaza el elemento en la posición 'n' de la lista 'L' con el elemento 'x' y devuelve una nueva lista.
(define (list-set L n x)
  (cond
    ;; Caso base 1: La lista 'L' es vacía.
    ;; Devuelve una lista vacía, por que no se puede reemplazar ya que es vacia.
    [(null? L) '()]
    
    ;; Caso base 2: Se ha alcanzado la posición 'n' en la lista.
    ;; Devuelve una nueva lista donde el elemento en la posición 'n' se ha reemplazado por 'x'.
    [(= n 0) (cons x (cdr L))]
    
    ;; Caso recursivo: Todavía no se ha alcanzado la posición 'n'.
    ;; Continúa la recursión con el primer elemento de 'L' y reduce 'n' en 1.
    [else (cons (car L) (list-set (cdr L) (- n 1) x))]))

;pruebas dadas en el taller
;> (list-set '(a b c d) 2 '(1 2))
;(a b (1 2) d)
;> (list-set '(a b c d) 3 '(1 5 10))
;(a b c (1 5 10))





;punto 5

;; list-index: Encuentra el índice del primer elemento en una lista que cumple con una condición dada.
;;
;; Parámetros:
;;   P: Un procedimiento o función que toma un elemento de la lista como argumento y devuelve un valor booleano.
;;   L: La lista en la que se busca el elemento.
;;
;; Devuelve:
;;   El índice (basado en cero) del primer elemento en la lista L que cumple con la condición especificada por P.
;;   Devuelve #f si no se encuentra ningún elemento que cumpla con la condición.
;;
;; Ejemplo de uso:
;; (list-index (lambda (x) (= x 3)) '(1 2 3 4 5)) devuelve 2
;;
(define (list-index P L)
  (cond
    ;; Caso 1: Si la lista L es vacía, devuelve #f, ya que no hay elementos que cumplan la condición en una lista vacía.
    ((null? L) #f)
    
    ;; Caso 2: Si la condición P aplicada al primer elemento de la lista (car L) es verdadera, devuelve 0,
    ;; indicando que el elemento se encuentra en la posición 0 (basada en cero) de la lista.
    ((P (car L)) 0)
    
    ;; Caso 3: Si no se cumple ninguno de los casos anteriores,
    ;; se llama recursivamente a list-index en el resto de la lista (cdr L).
    ;; Si el resultado de esta llamada recursiva no es #f, se suma 1 al resultado y se devuelve como el índice.
    ;; Si el resultado es #f, se devuelve #f, ya que no se encontró ningún elemento que cumpla la condición.
    (else
     (let ((rest-result (list-index P (cdr L))))
       (if (not (eq? rest-result #f))
           (+ 1 rest-result)
           #f)))))








;punto numero 6

;;primero definimos la funcion...
;; swapper: elemento elemento lista -> lista
;; Intercambia los elementos 'E1' y 'E2' en la lista 'L' y devuelve una nueva lista con los elementos intercambiados.
(define (swapper E1 E2 L)
  ;; elemento:  elemento -> elemento
  ;; Intercambia 'E1' por 'E2' o viceversa en un elemento.
  (define (elemento el)
    (cond
      [(equal? el E1) E2]   ; Si 'el' es igual a 'E1', lo reemplaza por 'E2'
      [(equal? el E2) E1]   ; Si 'el' es igual a 'E2', lo reemplaza por 'E1'
      [else el]))           ; En otros casos, devuelve 'el' sin cambios

  ;; intercambio-list: lista -> lista
  ;; Aplica elemento a cada elemento de la lista 'lst' y construye una nueva lista.
  (define (intercambio-list lst)
    (cond
      [(null? lst) '()]  ; Caso base: lista vacía, devuelve una lista vacía
      [else (cons (elemento (car lst)) (intercambio-list (cdr lst)))])) ; Recursión

  (intercambio-list L))  ; Llamada inicial a la función auxiliar swap-list

;pruebas dadas en el taller 
;> (swapper 'a 'd '(a b c d))
;(d b c a)
;> (swapper 'a 'd '(a d () c d))
;(d a () c a)
;> (swapper 'x 'y '(y y x y x y x x y))
;(x x y x y x y y x)




;punto 8



;; mapping: Realiza una operación de mapeo entre dos listas, aplicando una función F a los elementos de la primera lista L1
;;          y comparando los resultados con los elementos de la segunda lista L2.
;;
;; Parámetros:
;;   F: Una función que toma un elemento de L1 como argumento y devuelve un valor.
;;   L1: La primera lista que se va a mapear.
;;   L2: La segunda lista que se va a utilizar para comparar los resultados del mapeo.
;;
;; Devuelve:
;;   Una lista que contiene pares de elementos de L1 y L2, donde un par (x, y) está presente en la lista de salida
;;   si y solo si (F x) es igual a y. Los elementos se colocan en el mismo orden que en L1 y L2.
;;   Si alguna de las listas L1 o L2 es vacía, o si no se encuentra ninguna correspondencia, devuelve una lista vacía ().
;;

(define (mapping F L1 L2)
  (cond
    ;; Caso 1: Si una de las dos listas (L1 o L2) es vacía, devuelve una lista vacía (),
    ;; ya que no hay elementos para mapear o comparar.
    ((or (null? L1) (null? L2)) '())
    
    ;; Caso 2: Si (F (car L1)) es igual al primer elemento de L2 (car L2),
    ;; entonces hay una correspondencia y se crea un par (car L1, car L2).
    ;; Luego, se llama recursivamente a mapping en el resto de las listas (cdr L1) y (cdr L2),
    ;; y se concatena el par resultante con la lista resultante de la llamada recursiva.
    ((= (F (car L1)) (car L2))
     (cons (list (car L1) (car L2))
           (mapping F (cdr L1) (cdr L2))))
    
    ;; Caso 3: Si no se cumple ninguno de los casos anteriores,
    ;; se llama recursivamente a mapping en el resto de las listas (cdr L1) y (cdr L2)
    ;; sin agregar ningún nuevo par a la lista resultante.
    (else
     (mapping F (cdr L1) (cdr L2)))))










;;punto numero 9


;; Definición de la función inversions que cuenta el número de inversiones en una lista L.
(define (inversions L)
  ;; Función auxiliar invertir-conteo para contar las inversiones en una lista.
  (define (invertir-conteo lst n)
    ;; Si la lista está vacía, retorna el contador n que contiene el número de inversiones.
    (if (null? lst)
        n
        ;; Llamamos recursivamente a invertir-conteo con la cola de la lista (cdr lst) y actualizamos el contador n.
        ;; Luego, sumamos el resultado de count-greater para el primer elemento de la lista (car lst) y la cola de la lista (cdr lst).
        (invertir-conteo (cdr lst)
                          (+ n (conteo-mayor (car lst) (cdr lst))))))

  ;; Función auxiliar count-greater para contar cuántos elementos en lst son mayores que x.
  (define (conteo-mayor x lst)
    ;; Si la lista está vacía, no hay elementos mayores, por lo que retorna 0.
    (if (null? lst)
        0
        ;; Comparamos x con el primer elemento de la lista (car lst).
        ;; Si x es mayor, incrementamos el contador en 1 y llamamos recursivamente a count-greater con x y la cola de la lista (cdr lst).
        ;; Si no es mayor, simplemente llamamos recursivamente a count-greater con x y la cola de la lista (cdr lst).
        (if (> x (car lst))
            (+ 1 (conteo-mayor x (cdr lst)))
            (conteo-mayor x (cdr lst)))))

  ;; Llamamos a la función auxiliar count-inversions con la lista L y 0 como contador inicial.
  (invertir-conteo L 0))


;pruebas dadas en el taller
; (inversions '(2 3 8 6 1))
;5
;(inversions '(1 2 3 4))
;0
;(inversions '(3 2 1))
;3



;punto 11



(define (zip F L1 L2)
  ;; La función 'zip' toma tres argumentos: F, L1 y L2.
  ;; F es una función que tomará dos elementos como argumentos y realizará alguna operación.
  ;; L1 y L2 son dos listas que se combinarán elemento por elemento usando la función F.

  (if (or (null? L1) (null? L2))
      ;; Si alguna de las dos listas está vacía, retornamos una lista vacía como resultado.
      '()
      ;; En caso contrario, realizamos lo siguiente:
      (cons (F (car L1) (car L2))
            ;; - Tomamos el primer elemento de L1 (car L1) y el primer elemento de L2 (car L2),
            ;;   y aplicamos la función F a estos dos elementos.
            (zip F (cdr L1) (cdr L2)))))
            ;; - Luego, recursivamente llamamos a la función 'zip' con los elementos restantes de L1 y L2
            ;;   (es decir, sin el primer elemento), y concatenamos el resultado con el resultado anterior
            ;;   utilizando 'cons'.

;pruebas dadas en el taller
;> (zip + ’(1 4) ’(6 2))
;(7 6)
;> (zip * ’(11 5 6) ’(10 9 8))
;(110 45 48)









;punto 12


(define (filter-acum a b F acum filter)
  ;; La función filter-acumx toma cinco parámetros:
  ;; a: El valor inicial del rango
  ;; b: El valor final del rango
  ;; F: Una función binaria que se aplica a los elementos que pasen el filtro
  ;; acum: El valor acumulado inicial
  ;; filter: Una función unaria que se utiliza como filtro para seleccionar elementos del rango
  (define (iterar-valor valor-actual accum)
    ;; La función iterar-valor es una función auxiliar interna
    ;; Toma dos parámetros:
    ;; valor-actual: El valor actual que estamos evaluando en el rango [a,b]
    ;; accum: El acumulador actual que lleva el resultado
    (cond
      ((> valor-actual b) accum)
      ;; Si el valor-actual es mayor que el límite superior b, hemos terminado
      ;; Devolvemos el acumulador actual como resultado final
      ((filter valor-actual)
       ;; Si el valor-actual pasa el filtro proporcionado
       ;; aplicamos la función binaria F a valor-actual y acumulador
       ;; y luego llamamos recursivamente a iterar-valor con el siguiente valor-actual
       (iterar-valor (+ valor-actual 1) (F valor-actual accum)))
      (else
       ;; Si el valor-actual no pasa el filtro simplemente pasamos al siguiente valor-actual
       ;; sin modificar el acumulador y llamamos recursivamente a iterar-valor
       (iterar-valor (+ valor-actual 1) accum))))
  ;; Iniciamos la recursión llamando a iterar-valor con los valores iniciales a y acum
  (iterar-valor a acum))

;pruebas dadas en el taller
;> (filter-acum 1 10 + 0 odd?)
;25
;> (filter-acum 1 10 + 0 even?)
;30






;punto 14


(define (make-tree value left right)
  ;; Esta función crea un nuevo nodo de un árbol binario de búsqueda (BST) con los siguientes argumentos:
  ;; - 'value': el valor que se almacena en el nodo.
  ;; - 'left': el subárbol izquierdo (también puede ser un nodo vacío).
  ;; - 'right': el subárbol derecho (también puede ser un nodo vacío).
  ;; Retorna una lista que representa el nodo con su valor y sus subárboles izquierdo y derecho.
  (list value left right))

(define (value tree)
  ;; Esta función toma un nodo 'tree' y devuelve el valor almacenado en ese nodo.
  (car tree))

(define (left tree)
  ;; Esta función toma un nodo 'tree' y devuelve el subárbol izquierdo de ese nodo.
  (cadr tree))

(define (right tree)
  ;; Esta función toma un nodo 'tree' y devuelve el subárbol derecho de ese nodo.
  (caddr tree))

(define (empty? tree)
  ;; Esta función verifica si un nodo 'tree' es un nodo vacío.
  ;; Retorna #t si el nodo es vacío (una lista vacía), o #f si no lo es.
  (null? tree))

(define (path n BST)
  ;; Esta función toma un valor 'n' y un árbol binario de búsqueda (BST).
  ;; Retorna una lista que representa el camino para encontrar el valor 'n' en el árbol.
  (cond
    ((empty? BST) '()) ;; Si el árbol es vacío, no se encuentra 'n', por lo que retorna una lista vacía.
    ((= n (value BST)) '()) ;; Si el valor 'n' coincide con el valor en el nodo actual, el camino es una lista vacía.
    ((< n (value BST)) ;; Si 'n' es menor que el valor en el nodo actual, debemos ir a la izquierda.
     (cons 'left (path n (left BST)))) ;; Concatenamos 'left' a la lista que representa el camino y recursivamente buscamos en el subárbol izquierdo.
    (else ;; Si 'n' es mayor que el valor en el nodo actual, debemos ir a la derecha.
     (cons 'right (path n (right BST)))) ;; Concatenamos 'right' a la lista que representa el camino y recursivamente buscamos en el subárbol derecho.
    )
  )




;punto numero 15 

(define (count-odd-and-even arbol)
  ;; Función auxiliar para contar pares e impares en un árbol binario
  (define (contar-par-impar-auxiliar arbolito)
    ;; Caso base: si el árbol está vacío, retorna una lista con ceros para pares e impares.
    (cond
      [(null? arbolito) '(0 0)]
      [(pair? arbolito) ; Si el nodo del arbolito  es una lista (nodo interno)
       ;; Divide el árbol en tres partes (valor actual, subárbol izquierdo y subárbol derecho).
       (let* ((valor (car arbolito))
              (sub-izquierdo (cadr arbolito))
              (sub-derecho (caddr arbolito))
              ;; Llama recursivamente a contar-par-impar-auxiliar en los subárboles izquierdo y derecho.
              (contador-izq (contar-par-impar-auxiliar sub-izquierdo))
              (contador-der (contar-par-impar-auxiliar sub-derecho))
              ;; Calcula los nuevos contadores sumando los valores actuales y los de los subárboles.
              (contador-actual (if (odd? valor) '(0 1) '(1 0))))
         (list (+ (car contador-izq) (car contador-der) (car contador-actual))
               (+ (cadr contador-izq) (cadr contador-der) (cadr contador-actual))))]
      [else ; Si el nodo no es una lista (un número)
       (if (odd? arbolito) '(0 1) '(1 0))])) ; Si es impar, incrementa el contador de impares en 1, de lo contrario en 0

  ;; Llama a la función auxiliar con el árbol dado y retorna el resultado.
  (contar-par-impar-auxiliar arbol))


;pruebas dadas en el taller
;(display (count-odd-and-even '(14 (7 () (12 () ()))
;                               (26 (20 (17 () ()) ())
;                               (31 () ())))))
;
;(display (count-odd-and-even '(14 (7 () (12 () ()))(26 (20 (17 () ()) ())(31 () ())))))
;
; Debería imprimir (4 3)








;punto 17


(define (prod-scalar-matriz mat vec)
  ;; Esta función toma una matriz 'mat' y un vector 'vec' y calcula su producto escalar.
  ;; Retorna un nuevo vector que es el resultado del producto escalar de 'mat' y 'vec'.
  
  (define (producto_escalable row vec)
    ;; Esta función toma una fila 'row' de la matriz y un vector 'vec'.
    ;; Calcula el producto elemento por elemento de la fila y el vector.
    ;; Retorna una lista que representa la fila resultante del producto escalar.
    
    (cond
      ((null? row) '()) ; Si la fila es vacía, el resultado es una lista vacía.
      (else (cons (* (car row) (car vec)) (producto_escalable (cdr row) (cdr vec))))))

  (define (multiplicar mat vec)
    ;; Esta función toma una matriz 'mat' y un vector 'vec'.
    ;; Calcula el producto escalar de 'mat' y 'vec' aplicando la función 'producto_escalable' a cada fila de 'mat'.
    ;; Retorna una lista de filas, donde cada fila es el resultado del producto escalar de 'mat' y 'vec'.

    (cond
      ((null? mat) '()) ; Si la matriz está vacía, el resultado es una lista vacía.
      (else (cons (producto_escalable (car mat) vec) (multiplicar (cdr mat) vec)))))

  (multiplicar mat vec)
)









;punto numero 18


;; Función para generar la fila N del triángulo de Pascal
(define (pascal N)
  ;; Función interna para generar una fila del triángulo de Pascal a partir de la fila anterior
  (define (genera-fila fila-anterior)
    ;; Función auxiliar para calcular los elementos de una fila a partir de la fila anterior
    (define (auxiliar fila-anterior)
      ;; Verifica si la fila anterior está vacía
      (if (null? fila-anterior)
          '() ; Si está vacía, retorna una lista vacía
          (cons (+ (car fila-anterior) ; Suma el primer elemento de la fila anterior
                   (if (null? (cdr fila-anterior)) 0 (cadr fila-anterior))) ; Suma el segundo elemento de la fila anterior si existe, de lo contrario, suma cero
                (auxiliar (cdr fila-anterior))))) ; Llama recursivamente a la función para el resto de la fila anterior

    ;; Retorna una nueva fila del triángulo de Pascal con un "1" al principio seguido de los elementos generados por la función auxiliar
    (cons 1 (auxiliar fila-anterior)))

  ;; Función recursiva para calcular la fila N del triángulo de Pascal
  (define (fila-pascal N fila-actual)
    ;; Verifica si N es igual a 0, esto es el caso base
    (if (= N 0)
        fila-actual ; Si es igual a 0, retorna la fila actual 
        (fila-pascal (- N 1) (genera-fila fila-actual)))) ; Si no es igual a 0, llama recursivamente a la función para obtener la siguiente fila

  ;; Verifica si N es igual a 0 un caso especialmente particular 
  (if (= N 0)
      '(1) ; Si N es 0, retorna una lista con un solo elemento "1" , representa en este caso la  primera fila 
      (fila-pascal N '()))) ; Si N no es 0, inicia el cálculo de la fila N llamando a la función fila-pascal con N y una fila inicial vacía

;pruebas dadas en el taller
;> (pascal 5)
;(1 4 6 4 1)
;> (pascal 1)
;(1)



