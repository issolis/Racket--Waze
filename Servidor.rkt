#lang racket

;----------------------------------------------------------Algoritmo Dijkstra--------------------------------------------------------

;Esta función se encarga de concluir si un elemento le pertenece o no a una lista
;Devuelve true o false, en caso que de que pertenezca devuelve true (#t)

(define (member? element list)
  (cond ((null? list) #f)
        ((equal? element (car list)) #t) ; 
        (else (member? element (cdr list)))
         )
  )

;Esta función busca la ruta más pequeña entre dos puntos en un grafo, utilizando la lógica del algoritmo dijkstra
;Devuelve una lista con la ruta más corta, por ejemplo '(1 2 3)
(define (findRoute inicialNode finalNode direccionsList blockedList graph ) 
  (cond ((member? finalNode blockedList)  (buildRoute  (car (car direccionsList)) finalNode direccionsList)) ; valida si ya se llegó al nodo final
        ((equal? inicialNode -1) '()) ; valida si ya no hay más rutas disponibles 
        (else (findRoute (min direccionsList blockedList -1 2 -1) ;se busca el nodo más pequeño
                         finalNode
                         (buildDireccions direccionsList
                                          (direccionsNode (min direccionsList blockedList -1 2 -1)
                                                          graph
                                                          blockedList
                                                          '()
                                                          (min direccionsList blockedList -1 1 -1))
                                          (min direccionsList blockedList -1 2 -1)
                                          blockedList ); se buscan las nuevas posibles direcciones
                         (append blockedList (list (min direccionsList blockedList -1 2 -1) )) ; se agregan los nodos ya visitados
                         graph
                         ))
        )
        )
;Esta función busca ya sea el nodo menor, o el peso del nodo menor
;devuelve un nodo, que puede ser un int o un string, o un peso que es un int
(define (min direccionsList blockedList numMin option nodeMin)
  (cond ((null? direccionsList) (cond ((equal? option 1) numMin)(else nodeMin) )) ; devuelve el nodo o el peso
        ((or (and (>= numMin (car(cdr (cdr (car direccionsList)))) )
              (not (member? (car(car direccionsList)) blockedList ) ) ) (and (= -1 numMin) (not (member? (car(car direccionsList)) blockedList ) )))
         (min (cdr direccionsList) blockedList  (car(cdr (cdr (car direccionsList)))) option (car (car direccionsList))) ) ; valida si hay un peso menor
        (else (min (cdr direccionsList) blockedList numMin option nodeMin))
    ))
;esta función busca las nuevas posibles rutas desde algún nodo, para el caso, del nodo con menor peso
;devuelve una lista con los vecinos de un nodo
(define (buildDireccions direccionsList direccionNode node blockedList)
 (cond ((equal? node -1) direccionsList) ;significa que ya no hay nodos disponibles
        ((null? direccionNode ) direccionsList) ;condición de finalización, en donde ya se analizaron todos los nodos vecinos 
        (else (buildDireccions (updateList direccionsList (car direccionNode) '() blockedList) (cdr direccionNode) node blockedList)) ; actualiza las direcciones posibles
        ))
;esta función busca los vecinos de un nodo
;devuelve una lista con los nodos vecinos
(define (direccionsNode node graph blockedList lis addition)
  (cond  ((null? graph) lis) ;condición de finalización, si ya se busco por todo el grafo acaba
         ((and (equal? node (car (cdr (car graph)))) (not (member?  (car(car graph)) blockedList)))
          (direccionsNode node
                          (cdr graph)
                          blockedList
                          (append lis (list (append  (list(car(car graph))) (list (car(cdr(car graph))))
                                                     (list (+ addition (car(cdr(cdr(car graph)))))) )))
                          addition))
         (else (direccionsNode node (cdr graph) blockedList lis addition) )
        ))

;Esta función se encarga de hacer el cambio de rutas, es decir, si de A-B pesa 5Km, y C-B pesa 3Km entonces cambia (B A 5) por (B C 3)
;devuelve una lista actualizada de rutas posibles
(define (updateList direccionList node newList blockedList)
  (cond ((null? direccionList) (append newList (list node)))
        ((and (not (member? (car (car direccionList)) blockedList))
              (equal? (car (car direccionList)) (car node) )
              (>  (car (cdr (cdr (car direccionList)))) (car(cdr(cdr node)))) )
         (append newList (list node) (cdr direccionList)))
        ((equal? (car direccionList) node) direccionList)
        (else (updateList (cdr direccionList) node (append newList (list(car direccionList))) blockedList))
        )
        )
;Esta función se encarga, apartir de todas las posibles listas, trazar desde el nodo final la ruta hasta el inicial
;devuelve la ruta en una lista como '(1 2 3)
(define (buildRoute inicialNode finalNode direccionsList)
  (append (list inicialNode)(auxbuildRoute inicialNode finalNode direccionsList '())))
(define (auxbuildRoute inicialNode finalNode direccionsList route)
  (cond ((equal? inicialNode finalNode) route)
        (else (auxbuildRoute inicialNode (findNode finalNode direccionsList) direccionsList (append (list finalNode) route))
    )))
;busca el nodo que apunta a otro nodo
;devuelve el nombre de un nodo
(define (findNode finalNode direccionsList)
  (cond ((equal? finalNode (car(car direccionsList))) (car(cdr(car direccionsList))) )
        (else (findNode finalNode (cdr direccionsList)))
    ))

;------------------------------------------Algoritmo para encontrar todas las rutas-----------------------------------------------------

;Esta función se encarga de encontrar todas las rutas entre dos puntos
;devuelve una lista con todas las rutas entre dos puntos
(define (allRoutes graph inicialNode finalNode)
 (finalProcess
  (getRoutes (auxallRoutes graph 0 1 (list (list inicialNode)) (list (list inicialNode)) )  inicialNode finalNode '())
  '()))

;Esta función encuentra todas las rutas desde un punto hasta cualquier punto del grafo, búsqueda por anchura
;devuelve todas las rutas desde un nodo
(define (auxallRoutes graph inicialLength finalLength routes lastRoutes)
  (cond ((equal? inicialLength finalLength) routes)
  (else(auxallRoutes graph
                  finalLength
                  (length (append routes (madeList lastRoutes graph '())))
                  (append routes (madeList lastRoutes graph '()))
                  (madeList lastRoutes graph '()) ))
    ))

;Esta función devuelve todas las rutas considerando los vecinos del último nodo de cada ruta
;devuelve una lista con rutas
(define (madeList routes graph newRoutes)
  (cond ((null? routes) newRoutes)
        (else (madeList (cdr routes) graph (append newRoutes (madeNewRoutes
                                                             (car routes)
                                                             (neighbor
                                                             (getElementByPosition (- (length (car routes)) 1 ) (car routes)) graph '())
                                                             '()
                                                             )))) 
    ))

;esta función devuelve todas las nuevas rutas dado solo una ruta, y considerando su nodo final
;devuelve una lista con rutas
(define (madeNewRoutes lis neighbors lists)
  (cond ((null? neighbors) lists)
        ((not (member? (car neighbors) lis)) (madeNewRoutes lis (cdr neighbors) (append lists (list (append lis (list (car neighbors)) )) ) ))
        (else (madeNewRoutes lis (cdr neighbors) lists))
   ))

;esta función obtiene los vecinos de un nodo
;devuelve una lista de nodos
(define (neighbor node graph neighbors)
  (cond ((null? graph) neighbors)
        ((equal? node (car (cdr (car graph)))) (neighbor node (cdr graph) (append neighbors (list (car (car graph))))) )
        (else (neighbor node (cdr graph) neighbors))
    ))
;esta función obtiene todas las rutas en las que se encuentra un nodo inicial y otro final
;devuelve todas esas rutas pero solo desde el inicial hasta el final
(define (getRoutes routes inicialNode finalNode finalRoutes)
  (cond ((null? routes) finalRoutes)
        ((member? finalNode (car routes)) (getRoutes (cdr routes) inicialNode finalNode (append finalRoutes (getRoute finalNode (car routes) '()))))
        (else (getRoutes (cdr routes) inicialNode finalNode finalRoutes))
        ))
;obtiene una ruta desde un nodo hasta otro dado otro ruta que ya los contenga
;devuelve un ruta
(define (getRoute element lis newList)
  (cond ((equal? element (car lis)) (list (append newList  (list (car lis)))))
        (else (getRoute element (cdr lis) (append newList  (list (car lis))) ))
         )
  )
;Las últimas funciones hacen que se repita una misma ruta, entonces esta función las coloca una única vez dado un grupo de rutas
;devuelve una lista de rutas
(define (finalProcess routes finalRoutes)
  (cond ((null? routes) finalRoutes)
        ((not (member? (car routes) finalRoutes)) (finalProcess (cdr routes) (append finalRoutes (list (car routes))) ))
        (else (finalProcess (cdr routes) finalRoutes))
        ))

;mide el largo de una ruta
;devuelve un numero entero
(define (length list)
  (auxLength list 0))
(define (auxLength list size)
  (cond ((null? list) size)
        (else (auxLength (cdr list) (+ 1 size) ))
        ))
;obtiene un elemento de una lista dada una posición
;devuelve un elemento
(define (getElementByPosition position list)
  (cond ((= 0 position) (car list))
        (else (getElementByPosition (- position 1) (cdr list)))
        ))

;-------------------------------------------------------------Procesamiento de datos-------------------------------------------------------------------
;El cliente envía algo como " 1 2 (2 1 3)" en donde 1 es la sección 1, 2 es la sección 2, y (2 1 3) es la sección 3, encuentra alguna de esas secciones
;devuelve un string
(define (getSeccion message seccion)
  (auxGetSeccion message 0 0 1 seccion 0))
(define (auxGetSeccion message start beggining end position counter)
  (cond ((equal? counter 3) (substring message start (- (string-length message) 2)))
        ((and (equal? (substring message  beggining end) " ") (equal? position counter) ) (substring message start (- end 1)))
        ((equal? (substring message  beggining end) " ") (auxGetSeccion message end (+ 1 beggining) (+ 1 end) position (+ 1 counter) ))
        (else (auxGetSeccion message start (+ 1 beggining) (+ 1 end) position counter))
        ))

;dado un string como (2 1 3) lo convierte en una arista como '(2 1 3)
;devuelve una lista
(define (madeNode node)
 (list (auxMadeNode node (string-length node) 0 0 1 '())))
(define (auxMadeNode node length  position auxiliarPos counter newNode)
  (cond ((equal? counter length) (append newNode (list (string->number (substring node position counter)))))
        ((equal? (substring node auxiliarPos counter) " ") (auxMadeNode node length  counter (+ 1 auxiliarPos) (+ 1 counter) (append newNode (list (substring node position (- counter 1))))) )
        (else (auxMadeNode node length  position (+ 1 auxiliarPos) (+ 1 counter) newNode) )
        ))
;dado un string como (2 1 3)(3 2 1) lo convierte en un conjunto de aristas como '((2 1 3)(3 2 1))
;devuelve una lista
(define (madeGraph abstractGraph)
  (auxMadeGraph abstractGraph (string-length abstractGraph) 1 0 1 '())) 
(define (auxMadeGraph abstractGraph length position auxiliarPos counter newGraph)
  (cond ((equal? counter length) (append newGraph (madeNode (substring abstractGraph position (- counter 1)))))
        ((equal? (substring abstractGraph auxiliarPos counter) ")" ) (auxMadeGraph
                                                                      abstractGraph
                                                                      length
                                                                      (+ 1 counter)
                                                                      (+ 1 auxiliarPos)
                                                                      (+ 1 counter)
                                                                      (append newGraph (madeNode (substring abstractGraph position (- counter 1))) ) ))
        (else (auxMadeGraph abstractGraph length position (+ 1 auxiliarPos) (+ 1 counter) newGraph))
        )
  )
;Esta función se encarga de llamar a la función findRoute o allRoutes dado el mensaje del cliente
;devuelve un string
(define (messageManagement message option)
   (cond ((equal? option 1) (format "~a" (findRoute (getSeccion message 1)
             (getSeccion message 2)
             (list (append (list (getSeccion message 1)) (list (getSeccion message 1)) '(0) ))
             '()
             
             (madeGraph (getSeccion message 3)))))
         (else (format "~a" (allRoutes (madeGraph (getSeccion message 3))
                          (getSeccion message 1)
                          (getSeccion message 2) )))
                            
         ))


;--------------------------------------------------servidor-------------------------------------------------------------
(require racket/tcp)

(define (handle-client client-input client-output  message)
  (displayln "Conexión establecida.")
  
;Lee el mensaje del cliente
  
  (displayln (string-append "Mensaje del cliente: " message))
  
;Envía una respuesta al cliente
  (displayln "Enviando respuesta al cliente...")
  (write (messageManagement message 1) client-output) ; Respuesta predeterminada
  (displayln (string-append "Todas las rutas son: " (messageManagement message 2)))
  (flush-output client-output)
  
  (displayln "Conexión cerrada.")
  
  
  (close-input-port client-input)
  (close-output-port client-output))

;Esta función inicia el server y deja un loop a la espera de ingreso de información
(define (start-server port)
  (define listener (tcp-listen port 5 #t))
  (displayln (string-append "Servidor escuchando en el puerto " (number->string port)))
  (let loop ()
    (define-values (client-input client-output) (tcp-accept listener))
    (thread (λ () (handle-client client-input client-output (read-line client-input))))
    (loop)))

;El main se encarga de llamar a la función que inicia al server, a la espera de mensajes
(define (main)
  (displayln "Esperando conexiones...")
  (start-server 8080))

(main) 