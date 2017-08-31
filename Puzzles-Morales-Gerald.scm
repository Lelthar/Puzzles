; (load "/home/gerald/Documents/gitHub/PuzzlesProyecto/Puzzles/Puzzles-Morales-Gerald.scm")

(require-extension srfi-13)

; La sopa de letras silvestre lista, funcion de generar se llama generarSopaLetras y recibe X y Y y una lista de palabras
; la funcion de resolver se llama 
;
;
;
;


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

;calcula el car de un string
(define (carStr palabra)
	(substring palabra 0 1)
)

(define (stringPosicion palabra valor)
	(cond
		((zero? valor) (carStr palabra))
		(#t (stringPosicion (cdrStr palabra) (sub1 valor)))
	)
)

;devuelve una lista con un x y y, que le pasen
(define (agregarXY x y)
	(cons x (cons y '()))
)
;-----------------------------------------------------------------------------------------
;--------------------------------- Funciones del puzzle 1 --------------------------------
;-----------------------------------------------------------------------------------------

;prueba (colocarPalabra? 1 0 '((@ @ @ @ @) (@ @ @ @ @) (@ "o" @ @ @) (@ @ @ @ @) (@ @ @ @ @)) 4 "hola" 3)
;(posicion3 1 0 '((@ @ @ @ @) ("h" @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @)) 4 "hola")

;Esta funcion retorna #t o #f dependiendo si una palabra se puede poner en la matriz
(define (colocarPalabra? x y matriz largo palabra posicion)
	(cond
		((and (= posicion 0) (>= (- x (sub1 largo)) 0)) (posicion0 x y matriz largo palabra))
		((and (= posicion 1) (>= (- x (sub1 largo)) 0) (< (+ y (sub1 largo)) (length (car matriz)))) (posicion1 x y matriz largo palabra))
		((and (= posicion 2) (< (+ y (sub1 largo)) (length (car matriz)))) (posicion2 x y matriz largo palabra))
		((and (= posicion 3) (< (+ x (sub1 largo)) (length matriz)) (< (+ y (sub1 largo)) (length (car matriz)))) (posicion3 x y matriz largo palabra))
		((and (= posicion 4) (< (+ x (sub1 largo)) (length matriz))) (posicion4 x y matriz largo palabra))
		((and (= posicion 5) (< (+ x (sub1 largo)) (length matriz)) (>= (- y (sub1 largo)) 0)) (posicion5 x y matriz largo palabra))
		((and (= posicion 6) (>= (- y (sub1 largo)) 0)) (posicion6 x y matriz largo palabra))
		((and (= posicion 7) (>= (- x (sub1 largo)) 0) (>= (- y (sub1 largo)) 0)) (posicion7 x y matriz largo palabra))
		(#t #f)
	)
)

;funcion auxiliar de colocarPalabra?
(define (posicion0 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion0 (sub1 x) y matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

;funcion auxiliar de colocarPalabra?
(define (posicion1 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion1 (sub1 x) (add1 y) matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

;funcion auxiliar de colocarPalabra?
(define (posicion2 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion2 x (add1 y) matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

;funcion auxiliar de colocarPalabra?
(define (posicion3 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion3 (add1 x) (add1 y) matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

;funcion auxiliar de colocarPalabra?
(define (posicion4 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion4 (add1 x) y matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

;funcion auxiliar de colocarPalabra?
(define (posicion5 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion5 (add1 x) (sub1 y) matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

;funcion auxiliar de colocarPalabra?
(define (posicion6 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion6 x (sub1 y) matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

;funcion auxiliar de colocarPalabra?
(define (posicion7 x y matriz largo palabra)
	(cond 
		((string-null? palabra) #t)
		((or (eq? '@ (elementoPosicionXY x y matriz)) (string= (carStr palabra) (elementoPosicionXY x y matriz))) (posicion7 (sub1 x) (sub1 y) matriz (sub1 largo) (cdrStr palabra)))
		(#t #f)
	)	
)

(define (agregarPalabras matriz listaPalabras)
	(cond
		((null? listaPalabras) matriz)
		(#t (agregarPalabras (agregarPalabraMatriz matriz (car listaPalabras) 100 (random 8) (random (length matriz)) (random (length (car matriz)))) (cdr listaPalabras)))
	)

)

(define (agregarPalabraMatriz matriz palabra valor posicion x y)
	(cond
		((zero? valor) matriz)
		((colocarPalabra? x y matriz (string-length palabra) palabra posicion) (insertarPalabra x y matriz (string-length palabra) palabra posicion))
		(#t (agregarPalabraMatriz matriz palabra (sub1 valor) (random 8) (random (length matriz)) (random (length (car matriz)))))
	)
)

;(colocarPalabra? (random (length '((@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @)))) (random (length (car '((@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @))))) '((@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @)) (string-length "hola") "hola" 3)
;(agregarPalabraMatriz '((@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @)) "hola" 10 3 1 2)
;(agregarPalabras '((@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @) (@ @ @ @ @)) '("hola" "mudo" "casa" "cazar"))

(define (insertarPalabra x y matriz largo palabra posicion)
	(cond
		((= posicion 0) (insertarPosicion0 x y matriz largo palabra))
		((= posicion 1) (insertarPosicion1 x y matriz largo palabra))
		((= posicion 2) (insertarPosicion2 x y matriz largo palabra))
		((= posicion 3) (insertarPosicion3 x y matriz largo palabra))
		((= posicion 4) (insertarPosicion4 x y matriz largo palabra))
		((= posicion 5) (insertarPosicion5 x y matriz largo palabra))
		((= posicion 6) (insertarPosicion6 x y matriz largo palabra))
		((= posicion 7) (insertarPosicion7 x y matriz largo palabra))
	)
)

(define (insertarPosicion0 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion0 (sub1 x) y (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

;funcion auxiliar de colocarPalabra?
(define (insertarPosicion1 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion1 (sub1 x) (add1 y) (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

;funcion auxiliar de colocarPalabra?
(define (insertarPosicion2 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion2 x (add1 y) (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

;funcion auxiliar de colocarPalabra?
(define (insertarPosicion3 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion3 (add1 x) (add1 y) (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

;funcion auxiliar de colocarPalabra?
(define (insertarPosicion4 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion4 (add1 x) y (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

;funcion auxiliar de colocarPalabra?
(define (insertarPosicion5 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion5 (add1 x) (sub1 y) (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

;funcion auxiliar de colocarPalabra?
(define (insertarPosicion6 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion6 x (sub1 y) (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

;funcion auxiliar de colocarPalabra?
(define (insertarPosicion7 x y matriz largo palabra)
	(cond 
		((string-null? palabra) matriz)
		(#t (insertarPosicion7 (sub1 x) (sub1 y) (remplazarAtom x y (carStr palabra) matriz) (sub1 largo) (cdrStr palabra)))
	)	
)

(define (llenarMatriz matriz palabras)
	(llenarMatrizAux matriz palabras (length matriz))
)
(define (llenarMatrizAux matriz palabras largo)
	(cond
		((zero? largo) '())
		(#t (cons (llenarMatrizFila (car matriz) (string-concatenate palabras) (length (car matriz))) (llenarMatrizAux (cdr matriz) palabras (sub1 largo))))
	)
)

(define (llenarMatrizFila fila palabra largo)
	(cond
		((zero? largo) '())
		((eq? (car fila) '@) (cons (stringPosicion palabra (random (string-length palabra))) (llenarMatrizFila (cdr fila) palabra (sub1 largo))))
		(#t (cons (car fila)(llenarMatrizFila (cdr fila) palabra (sub1 largo))))
	) 
)

;Crea la sopa de letras
(define (generarSopaLetras x y listaPalabras)
	(llenarMatriz (agregarPalabras (generarMatriz x y) listaPalabras) listaPalabras)
)

(define (revisarPalabra? x y matriz largo palabra posicion)
	(cond
		((and (= posicion 0) (>= (- x (sub1 largo)) 0)) (revisarPalabraPosicion0 x y matriz largo palabra '()))
		((and (= posicion 1) (>= (- x (sub1 largo)) 0) (< (+ y (sub1 largo)) (length (car matriz)))) (revisarPalabraPosicion1 x y matriz largo palabra '()))
		((and (= posicion 2) (< (+ y (sub1 largo)) (length (car matriz)))) (revisarPalabraPosicion2 x y matriz largo palabra '()))
		((and (= posicion 3) (< (+ x (sub1 largo)) (length matriz)) (< (+ y (sub1 largo)) (length (car matriz)))) (revisarPalabraPosicion3 x y matriz largo palabra '()))
		((and (= posicion 4) (< (+ x (sub1 largo)) (length matriz))) (revisarPalabraPosicion4 x y matriz largo palabra '()))
		((and (= posicion 5) (< (+ x (sub1 largo)) (length matriz)) (>= (- y (sub1 largo)) 0)) (revisarPalabraPosicion5 x y matriz largo palabra '()))
		((and (= posicion 6) (>= (- y (sub1 largo)) 0)) (revisarPalabraPosicion6 x y matriz largo palabra '()))
		((and (= posicion 7) (>= (- x (sub1 largo)) 0) (>= (- y (sub1 largo)) 0)) (revisarPalabraPosicion7 x y matriz largo palabra '()))
		(#t '())
	)
)

(define (revisarPalabraPosicion0 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion0 (sub1 x) y matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (revisarPalabraPosicion1 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion1 (sub1 x) (add1 y) matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (revisarPalabraPosicion2 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion2 x (add1 y) matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (revisarPalabraPosicion3 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion3 (add1 x) (add1 y) matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (revisarPalabraPosicion4 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion4 (add1 x) y matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (revisarPalabraPosicion5 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion5 (add1 x) (sub1 y) matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (revisarPalabraPosicion6 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion6 x (sub1 y) matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (revisarPalabraPosicion7 x y matriz largo palabra resultado)
	(cond 
		((zero? largo) resultado)
		((equal? (elementoPosicionXY x y matriz) (carStr palabra)) (revisarPalabraPosicion7 (sub1 x) (sub1 y) matriz (sub1 largo) (cdrStr palabra) (cons (agregarXY x y) resultado)))
		(#t '())
	)
)

(define (evaluarElementoMatriz x y matriz largo palabra contador resultado)
	(cond
		((zero? contador) resultado)
		((null? resultado) (evaluarElementoMatriz x y matriz largo palabra (sub1 contador) (revisarPalabra? x y matriz largo palabra (sub1 contador))))
		(#t  resultado)
	)
)

(define (evaluarMatriz matriz palabra x resultado)
	(cond
		((zero? x) 
			(cond
				((> (length resultado) 0) (reverse (cons palabra resultado)))
				(#t (cons palabra '()))
			))
		((null? resultado) (evaluarMatriz matriz palabra (sub1 x) (evaluarFila matriz palabra (sub1 x) (length (car matriz)) resultado)))
		(#t (reverse(cons palabra resultado)))
	)
)

(define (evaluarFila matriz palabra x y resultado)
	(cond
		((zero? y) resultado)
		((null? resultado) (evaluarFila matriz palabra x (sub1 y) (evaluarElementoMatriz x (sub1 y) matriz (string-length palabra) palabra 8 '())))
		(#t resultado)
	)
)

;Resuelve una sopa de letras
(define (resolverSopaLetras matriz palabras)
	(cond
		((null? palabras) '())
		(#t (cons  (evaluarMatriz matriz (car palabras) (length matriz) '()) (resolverSopaLetras matriz (cdr palabras))))
	)
)

;----------------------------------------------------------------------------------------------------------------------
;-------------------------------------------------Sopa de letras Azura-------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------











