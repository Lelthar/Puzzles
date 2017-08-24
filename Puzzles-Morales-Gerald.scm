; (load "/home/gerald/Documents/Lenguajes-de-Programacion/Tarea-Mediana/Puzzles-Morales-Gerald.scm")

(require-extension srfi-13)

;Usar
;Esta funcion genera una matriz X x Y de elementos @ 
(define (generarMatriz x y)
	(cond
		((zero? x) '())
		(#t (cons (generarFila y)(generarMatriz (sub1 x) y)))
	)
)

;Esta funcion genera una fila con la cantidad que le pongan con el valor @
(define (generarFila y)
	(cond
		((zero? y) '())
		(#t (cons '@ (generarFila (sub1 y))))
	)
)

;Usar
;Esta funcion da el elemento en la posicion X y Y que le pongan
(define (elementoPosicionXY x y matriz)
	(elementoPosicionXYAux x y matriz 0)
)

;Esta funcion es el auxiliar de la funcion elementoPosicionXY
(define (elementoPosicionXYAux x y matriz cero)
	(cond
		((eq? x cero) (elementoFila y 0 (car matriz)))
		(#t (elementoPosicionXYAux x y (cdr matriz) (add1 cero)))
	)

)
;Esta funcion devuelve el elemento de una lista en la posicion y
(define (elementoFila y cero fila)
	(cond 
		((eq? y cero)(car fila))
		(#t (elementoFila y (add1 cero)(cdr fila)))
	)

)

;Auxiliar de la funcion de remplazar un atom
(define (remplazarAtomAux x y new matriz valor)
	(cond
		((eq? valor x) (cons (remplazarAtomFilaAux y new (car matriz) 0) (cdr matriz)))
		(#t (cons (car matriz) (remplazarAtomAux x y new (cdr matriz) (add1 valor))))
	)

)

;Auxiliar de la funcion de remplazar un atom
(define (remplazarAtomFilaAux y new fila valor)
	(cond 
		((and(null? (car fila))(eq? valor y)) '(new))
		((eq? valor y) (cons new (cdr fila)))
		(#t (cons (car fila) (remplazarAtomFilaAux y new (cdr fila) (add1 valor))))
	)
)


;Remplazo de atom matriz en la posicion x y
(define (remplazarAtom x y new matriz)
	(remplazarAtomAux x y new matriz 0)
)

;Calcula el cdr de un string
(define (cdrStr palabra)
	(substring palabra 1 (string-length palabra))
)

;-----------------------------------------------------------------------------------------

;Esta funcion retorna #t o #f dependiendo si una palabra se puede poner en la matriz
(define (ColocarPalabra? x y matriz largo palabra posicion)
	(cond
		((eq? posicion 0) #t)
		((eq? posicion 1) #t)
		((eq? posicion 2) #t)
		((eq? posicion 3) #t)
		((eq? posicion 4) #t)
		((eq? posicion 5) #t)
		((eq? posicion 6) #t)
		((eq? posicion 7) #t)
	)
)

(define (posicion0 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((eq? ))

	)	
)
