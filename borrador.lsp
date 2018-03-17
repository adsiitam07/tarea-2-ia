(setq hijos '((CDMX (-2 2)(Puebla 10 Hidalgo 8))(Guadalajara (-2 9) (Morelos 5 Puebla 2))
(Puebla (-3 5) (CDMX 10 Guadalajara 2))(Morelos (-3 6) (Guadalajara 5 Hidalgo 7)) 
(Hidalgo (-3 4) (Morelos 7 CDMX 8))))

(setq cerrado '())
(setq abierto '())
(setq respuesta '())

(defun obtenHijos(nombre lista)
	(cond
		((null lista) nil)
		((eql (caar lista) nombre) (remove nil (mapcar (lambda (x) (if (symbolp x) x)) (car (last (car lista))))))
		(t (obtenHijos nombre (cdr lista)))
	)
)

(defun longitudCamino(padre hijo lista)
	(cond
		((null lista) nil)
		((eql (caar lista) padre) (let ((aux (car (last (car lista)))))
										(loop
											(when (or (null aux) (eql (car aux) hijo)) (return (cadr aux)))
											(setq aux (cddr aux))
										)
									)
		)
		(t (longitudCamino padre hijo (cdr lista)))
	)
)


(defun distLinRec(ciudad1 ciudad2)
	(setq pto1 (car (remove nil (mapcar (lambda (x) (if (eql ciudad1 (car x)) (second x))) hijos))))
	(setq pto2 (car (remove nil (mapcar (lambda (x) (if (eql ciudad2 (car x)) (second x))) hijos))))
	(sqrt (+ (expt (- (first pto1) (first pto2)) 2) (expt (- (second pto1) (second pto2)) 2)))
)

(defun regresaSolucion(lista padre)
	(cond 
		((null lista) respuesta)
		((eql padre (caar lista)) (push padre respuesta) (regresaSolucion (cdr lista) (cadar lista)))
		(t (regresaSolucion (cdr lista) padre))
	)
)

(defun rbfs(destino actual flimite)
	(cond 
		((eql destino (car actual)) (regresaSolucion cerrado destino))
		(t 	
			(if (eql destino (car actual)) (regresaSolucion cerrado destino))
			(setq abierto (append (mapcar (lambda (x) (list x (car actual) (+ (distLinRec x (car actual)) (+ (longitudCamino (car actual) x hijos) (third actual))))) (obtenHijos(car actual) hijos)) abierto))
			(setq abierto (set-difference abierto cerrado :key 'car))
			(setq abierto (sort (copy-seq abierto) #'< :key #'third))
			(setq best (pop abierto))
			(push best cerrado)
			(rbfs destino best flimite))
	)
)

(defun aestrella(destino actual)
	(push (append (list actual) '( sin-padre 0)) cerrado)
	(rbfs destino (car cerrado) 999)
)
;Llegar de Morelos a CDMX
;Proto de nodo (CDMX sin-papa 0)
;actual = (CDMX sin-papa 0)
;abierto = '()
;cerrado = '()

(print (aestrella 'Morelos 'CDMX))