;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TP2-Albornoz-Chiminelli) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 2: Listas
Integrantes:
- [Albornoz, Martín].
- [Chiminelli, Gabriel].
|#

;---------------------Diseño de datos------------------------

; Utilizamos exact-round
(require racket/math)

; Representamos los nombres de los países con String y
; las fechas (año) y el número de habitantes de un país con Number.

(define-struct registro [Fecha Poblacion])
; registro es (Number, Number)
; Interpretación: un elemento en registro representa el número
; de habitantes de un país (Poblacion) en una determinada fecha (Fecha).

(define-struct pais [Nombre registros])
; pais es (String, List(registro))
; Interpretación: un elemento en pais representa un pais de nombre
; Nombre con su lista de registros de censo asociado registros.

;---------------------Constantes para casos de prueba------------------------

(define ANDORRA (make-pais "Andorra" (list (make-registro 2005 35000000)
                                           (make-registro 2006 -1))))
(define GUATEMALA (make-pais "Guatemala" (list (make-registro 2018 -1)
                                               (make-registro 2019 32500000))))
(define ARGENTINA (make-pais "Argentina" (list (make-registro 2014 35000000)
                                               (make-registro 2015 40000000))))
(define CHINA (make-pais "China" (list (make-registro 2018 999999956)
                                       (make-registro 2019 1000000045))))

;---------------------Preparación de los datos------------------------

; Datos de entrada
(define INPUT-PAISES (read-csv-file "BDPoblacional.csv"))
; Removemos el encabezado
(define DATOS-PAISES (rest INPUT-PAISES))

; Primer registro censo
(define FECHA-INICIO 1960)
; Ultimo registro censo
(define FECHA-FIN 2019)
; Años en los que se realizaron censos [1960,2019]
(define RANGO-FECHA (range FECHA-INICIO (+ FECHA-FIN 1) 1))

; armar-registros: List(Number) List(String) -> List(registro)
; armar-registros toma una lista de años *lf* y una lista de strings que representan números
; de habitantes *lnh* y devuelve una lista de registros *lr* en donde el i-ésimo
; elemento de *lr* se contruye a partir del i-ésimo elemento de *lf* y
; el i-ésimo elemento de *lnh* convertido a número
(check-expect (armar-registros (list 2005 2006) (list "3" "4"))
              (list (make-registro 2005 3) (make-registro 2006 4)))
(check-expect (armar-registros (list 2005 2006) empty) empty)
(check-expect (armar-registros empty (list (make-registro 2005 3))) empty)
(check-expect (armar-registros empty empty) empty)

(define (armar-registros lf lnh)
  (cond [(empty? lnh) empty]
        [(empty? lf) empty]
        [else (cons (make-registro (first lf) (string->number (first lnh)))
                    (armar-registros (rest lf) (rest lnh)))]))

; armar-lista-registros List(List(String)) List(Number) -> List(List(registro))
; armar-lista-registros toma una lista de listas de strings que representan cantidad de habitantes
; *lnh* y una lista de años *lf* y devuelve una lista de listas de registro *lr*
; Los campos Población de los elementos de *lr* coinciden con los elementos de *lnh*
; y su campo fecha se completa con los años de *lf*.
(check-expect (armar-lista-registros empty empty) empty)
(check-expect (armar-lista-registros (list (list "3" "4")) empty) empty)
(check-expect (armar-lista-registros empty (list 2005 2006)) empty)
(check-expect (armar-lista-registros
                  (list (list "3" "4")
                        (list "5" "6")
                        (list "7" "8"))
                  (list 2005 2006))
              (list (list (make-registro 2005 3) (make-registro 2006 4))
                    (list (make-registro 2005 5) (make-registro 2006 6))
                    (list (make-registro 2005 7) (make-registro 2006 8))))

(define (armar-lista-registros lnh lf)
  (cond [(empty? lnh) empty]
        [(empty? lf) empty]
        [else (cons (armar-registros lf (first lnh))
                    (armar-lista-registros (rest lnh) lf))]))

; Datos para construir registros (sin nombres de países)
(define LISTA-DATOS-REGISTRO (map rest DATOS-PAISES))
; Lista de registros de cada país
(define LISTA-REGISTROS (armar-lista-registros LISTA-DATOS-REGISTRO RANGO-FECHA))

; armar-paises: List(String) List(registro) -> List(pais)
; armar-paises toma una lista de nombres de países *lnp* y una lista de registros
; poblacionales *lr* y devuelve una lista de estructura pais *lp* en donde
; el i-ésimo elemento de *lp* tiene de campo Nombre el i-ésimo elemento
; de *lnp* y de campo registros el i-ésimo elemento de *lr*
(check-expect (armar-paises (list "Angola" "Andorra" "Argentina") empty) empty)
(check-expect (armar-paises empty (list (list (make-registro 2005 3) (make-registro 2006 4)))) empty)
(check-expect (armar-paises (list "Angola" "Andorra" "Argentina")
                            (list (list (make-registro 2005 3) (make-registro 2006 4))
                                  (list (make-registro 2005 5) (make-registro 2006 6))
                                  (list (make-registro 2005 7) (make-registro 2006 8))))
              (list (make-pais "Angola" (list (make-registro 2005 3) (make-registro 2006 4)))
                    (make-pais "Andorra" (list (make-registro 2005 5) (make-registro 2006 6)))
                    (make-pais "Argentina" (list (make-registro 2005 7) (make-registro 2006 8)))))

(define (armar-paises lnp lr)
  (cond [(empty? lnp) empty]
        [(empty? lr) empty]
        [else (cons (make-pais (first lnp) (first lr))
                    (armar-paises (rest lnp) (rest lr)))]))

; Nombres de países
(define LISTA-NOMBRE-PAISES (map first DATOS-PAISES))


; Lista de países
(define LISTA-PAISES (armar-paises LISTA-NOMBRE-PAISES LISTA-REGISTROS))

;---------------------Funciones de alto orden sobre listas de países------------------------

; transformar-paises: List(pais) (pais -> pais) -> List(pais)
(define (transformar-paises lp transformacion)
  (cond [(empty? lp) empty]
        [else (cons (transformacion (first lp))
                    (transformar-paises (rest lp) transformacion))]))

; filtrar-paises: List(pais) (pais -> Boolean)-> List(pais)
(define (filtrar-paises lp predicado)
  (cond [(empty? lp) empty]
        [else (if (predicado (first lp))
                  (cons (first lp) (filtrar-paises (rest lp) predicado))
                  (filtrar-paises (rest lp) predicado))]))

; operar-sobre-paises: List(pais) (pais X -> X) X -> X
(define (operar-sobre-paises lp operador neutro)
  (cond [(empty? lp) neutro]
        [else (operador (first lp) (operar-sobre-paises (rest lp) operador neutro))]))

;---------------------Consultas y actualizaciones------------------------

;---------------------------------------------
; Sección 1 - Datos incompletos y datos incorrectos
;---------------------------------------------

; registro-esta-incompleto: List(registro) -> Boolean
; registro-esta-incompleto devuelve #true si alguno de los registros en la lista está
; incompleto, comparando si el mismo es igual a -1. Si cuenta con todos sus registros
; (son todos distintos a -1), devuelve #false
(check-expect (registro-esta-incompleto (make-registro 2018 -1)) #t)     
(check-expect (registro-esta-incompleto (make-registro 2019 100002)) #f)

(define (registro-esta-incompleto r)   ; Hay que cambiarle el nombre a predicado-registro-incompleto ?
  (= (registro-Poblacion r) -1))

; pais-tiene-registro-completo: registro -> Boolean
; pais-tiene-registro-completo devuelve #true si no falta ningun registro en la lista de
; registros de un país.
; Si falta un registro (si registro-esta-incompleto devuelve #true) entonces
; pais-tiene-registro-completo devuelve #false
(check-expect (pais-tiene-registro-completo ANDORRA) #f)
(check-expect (pais-tiene-registro-completo ARGENTINA) #t)

(define (pais-tiene-registro-completo p)
  (empty? (filter registro-esta-incompleto (pais-registros p))))

; Algunos países no tienen datos oficiales completos. Removerlos de la lista de paises.
; Definir una función predicado-registro-incompleto, transformacion-registro-incompleto
; u operacion-registro-incompleto para pasarle como argumento a
; alguno de los patrones de alto orden


; LISTA-PAISES-REGISTRO-COMPLETO: List(List(paises)) -> List(List(paises))
; LISTA-PAISES-REGISTRO-COMPLETO toma una lista de paises con sus registros y devuelve
; una nueva lista de paises incluyendo aquellos que tienen sus registros completos.

(define LISTA-PAISES-REGISTRO-COMPLETO
  (filtrar-paises LISTA-PAISES pais-tiene-registro-completo))

; Algunos casos de test para LISTA-PAISES-REGISTRO-COMPLETO
; "West Bank and Gaza" no tiene que estar en el listado de países
; con registro completo pero sí tienen que estar en el listado de países
(check-expect (member "West Bank and Gaza" (map pais-Nombre LISTA-PAISES)) #t)
(check-expect (member "West Bank and Gaza" (map pais-Nombre LISTA-PAISES-REGISTRO-COMPLETO)) #f)

; Los censos del 2014 se calcularon mal. Aumentarlos un 10%.
; Trabajamos con la lista de países con registro completo

; Función auxiliar extra de ayuda, se recomienda utilizar
; recalculo: registro -> registro
; recalculo toma un registro e incrementa la población en un 10%
; si la fecha en la que se condujo el censo fue 2014
; En caso de que el incremento resulte en un número no entero,
; se redondea
(check-expect (recalculo (make-registro 2014 10))
              (make-registro 2014 11))
(check-expect (recalculo (make-registro 2016 10))
              (make-registro 2016 10))

(define (recalculo reg)
  (if (= (registro-Fecha reg) 2014)
      (make-registro (registro-Fecha reg) (exact-round (* 1.1 (registro-Poblacion reg))))
      (make-registro (registro-Fecha reg) (registro-Poblacion reg))))

; transformacion-recalcular: pais -> pais
; transformacion-recalcular toma un país y aumenta en 10% la población asociada
; al año 2014 en sus registros
(check-expect (transformacion-recalcular ARGENTINA)
              (make-pais "Argentina" (list (make-registro 2014 38500000)
                                           (make-registro 2015 40000000))))
(check-expect (transformacion-recalcular CHINA) CHINA)

(define (transformacion-recalcular pais)
  (make-pais (pais-Nombre pais) (map recalculo (pais-registros pais))))

(define LISTA-PAISES-RECALCULADA
  (transformar-paises LISTA-PAISES-REGISTRO-COMPLETO transformacion-recalcular))

; Algunos casos de test para LISTA-PAISES-RECALCULADA
; Aruba 2014 original: 103774
; Afghanistan 2014 original: 33370794
(check-expect (member (make-registro 2014 103774)
                      (pais-registros (first LISTA-PAISES-REGISTRO-COMPLETO))) #t)
(check-expect (member (make-registro 2014 33370794)
                      (pais-registros (second LISTA-PAISES-REGISTRO-COMPLETO))) #t)
(check-expect (member (make-registro 2014 114151)
                      (pais-registros (first LISTA-PAISES-RECALCULADA))) #t)
(check-expect (member (make-registro 2014 36707873)
                      (pais-registros (second LISTA-PAISES-RECALCULADA))) #t)

;---------------------------------------------
; Sección 2 - Países superpoblados
;---------------------------------------------

; Los países con una población mayor a 1000 millones de habitantes en el 2019
; son paises superpoblados
; IMPORTANTE: Trabajamos con la lista de paises con registro completo
; y los valores del 2014 recalculados generada en la sección 1.

; Constante para establecer el mínimo de habitantes
; que hace que un país sea superpoblado
(define MILMILLONES 1000000000)

; Función auxiliar extra de ayuda, se recomienda utilizar
; last: list -> any
; last toma una lista y devuelve el último elemento si es no vacia
; Caso contrario devuelve false
(check-expect (last empty) #f)
(check-expect (last (list "trabajo practico 2")) "trabajo practico 2")
(check-expect (last (list 1 2 3 4 5 6 7)) 7)

(define (last l)
  (cond [(empty? l) #f]
        [(= 1 (length l)) (first l)]
        [else (last (rest l))]))

; predicado-superpoblados pais -> Boolean
; predicado-superpoblados toma un país y devuelve #t en el caso que haya
; censado más de 1000 millones de habitantes en el 2019
; Suponemos que la última entrada de cada país se corresponde con el año 2019
(check-expect (predicado-superpoblados GUATEMALA) #f)
(check-expect (predicado-superpoblados CHINA) #t)

(define (predicado-superpoblados pais)
  (> 
    (registro-Poblacion (last (pais-registros pais)))
    MILMILLONES))

(define LISTA-PAISES-SUPERPOBLADOS
  (filtrar-paises LISTA-PAISES-RECALCULADA predicado-superpoblados))

; Nombres de los países superpoblados
(define NOMBRES-PAISES-SUPERPOBLADOS (map pais-Nombre LISTA-PAISES-SUPERPOBLADOS))

; Cantidad de países sobrepoblados
(define CANT-PAISES-SUPERPOBLADOS (length LISTA-PAISES-SUPERPOBLADOS))

; Porcentaje de población en países superpoblados:
; Suma de poblaciones de países superpoblados / población mundial
; Suponemos que la población mundial se calcula a partir de las poblaciones
; de todos los países de los cuales tenemos registro.

; operacion-sumar-poblaciones: List(pais) Number -> Number
; suma las poblaciones historicas validas (≠ -1) la lista de registros de un país a n

(check-expect (operacion-sumar-poblaciones ARGENTINA 0) 75000000)
(check-expect (operacion-sumar-poblaciones CHINA 0) 2000000001)
(check-expect (operacion-sumar-poblaciones GUATEMALA 0) 32500000)
(define (operacion-sumar-poblaciones pais n)
  (+ n
     (foldr + 0
            (map registro-Poblacion
                 (filter
                  (lambda (r) (not (registro-esta-incompleto r)))
                  (pais-registros pais))))))

(define TASA-POBLACION-SUPERPOBLADOS
  (/ (operar-sobre-paises LISTA-PAISES-SUPERPOBLADOS operacion-sumar-poblaciones 0)
     (operar-sobre-paises LISTA-PAISES operacion-sumar-poblaciones 0)))