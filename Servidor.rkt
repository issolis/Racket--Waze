#lang racket

;Algoritmo Dijkstra:
(define (member? element list)
  (cond ((null? list) #f)
        ((equal? element (car list)) #t)
        (else (member? element (cdr list)))
         )
  )
(define (findRoute inicialNode finalNode direccionsList blockedList graph )
  (cond ((member? finalNode blockedList)  (buildRoute  (car (car direccionsList)) finalNode direccionsList))
        ((equal? inicialNode -1) '())
        (else (findRoute (min direccionsList blockedList -1 2 -1)
                         finalNode
                         (buildDireccions direccionsList
                                          (direccionsNode (min direccionsList blockedList -1 2 -1)
                                                          graph
                                                          blockedList
                                                          '()
                                                          (min direccionsList blockedList -1 1 -1))
                                          (min direccionsList blockedList -1 2 '())
                                          blockedList )
                         (append blockedList (list (min direccionsList blockedList -1 2 -1) ))
                         graph
                         ))
        )
        )
(define (min direccionsList blockedList numMin option nodeMin)
  (cond ((null? direccionsList) (cond ((equal? option 1) numMin)(else nodeMin) ))
        ((or (and (>= numMin (car(cdr (cdr (car direccionsList)))) )
              (not (member? (car(car direccionsList)) blockedList ) ) ) (and (= -1 numMin) (not (member? (car(car direccionsList)) blockedList ) )))
         (min (cdr direccionsList) blockedList  (car(cdr (cdr (car direccionsList)))) option (car (car direccionsList))) ) 
        (else (min (cdr direccionsList) blockedList numMin option nodeMin))
    ))
(define (buildDireccions direccionsList direccionNode node blockedList)
 (cond ((equal? node -1) direccionsList)
        ((null? direccionNode ) direccionsList)
        (else (buildDireccions (updateList direccionsList (car direccionNode) '() blockedList) (cdr direccionNode) node blockedList))
        ))
(define (direccionsNode node graph blockedList lis addition)
  (cond  ((null? graph) lis)
         ((and (equal? node (car (cdr (car graph)))) (not (member?  (car(car graph)) blockedList)))
          (direccionsNode node
                          (cdr graph)
                          blockedList
                          (append lis (list (append  (list(car(car graph))) (list (car(cdr(car graph))))
                                                     (list (+ addition (car(cdr(cdr(car graph)))))) )))
                          addition))
         (else (direccionsNode node (cdr graph) blockedList lis addition) )
        ))

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
(define (buildRoute inicialNode finalNode direccionsList)
  (append (list inicialNode)(auxbuildRoute inicialNode finalNode direccionsList '())))
(define (auxbuildRoute inicialNode finalNode direccionsList route)
  (cond ((equal? inicialNode finalNode) route)
        (else (auxbuildRoute inicialNode (findNode finalNode direccionsList) direccionsList (append (list finalNode) route))
    )))
(define (findNode finalNode direccionsList)
  (cond ((equal? finalNode (car(car direccionsList))) (car(cdr(car direccionsList))) )
        (else (findNode finalNode (cdr direccionsList)))
    ))

;Algoritmo para encontrar todas las rutas
(define (findAllRoutes inicialNode finalNode directionsList graph routes )
  (cond ((equal? (findRoute inicialNode finalNode directionsList '() graph ) '()) routes )
        (else (findAllRoutes inicialNode
                             finalNode
                             directionsList
                             (newGraph graph (findRoute inicialNode finalNode directionsList '() graph ) '())
                             (append routes (list (findRoute inicialNode finalNode directionsList '() graph )))
                             ))
        ))
(define (newGraph graph route newgraph)
  (cond ((equal? route '()) graph)
        ((null? graph) newGraph)
        ((and (equal? (car (car graph)) (getElementByPosition (- (length route) 2) route))
              (equal? (car(cdr (car graph))) (getElementByPosition (- (length route) 3) route))) (append newgraph (cdr graph)) )
        (else (newGraph (cdr graph) route (append newgraph (list (car graph)) ) ) )
    ))

(define (length list)
  (auxLength list 0))
(define (auxLength list size)
  (cond ((null? list) size)
        (else (auxLength (cdr list) (+ 1 size) ))
        ))
(define (getElementByPosition position list)
  (cond ((= 0 position) (car list))
        (else (getElementByPosition (- position 1) (cdr list)))
        ))

;Procesamiento de datos
(define (getSeccion message seccion)
  (auxGetSeccion message 0 0 1 seccion 0))

(define (auxGetSeccion message start beggining end position counter)
  (cond ((equal? counter 3) (substring message start (- (string-length message) 2)))
        ((and (equal? (substring message  beggining end) " ") (equal? position counter) ) (substring message start (- end 1)))
        ((equal? (substring message  beggining end) " ") (auxGetSeccion message end (+ 1 beggining) (+ 1 end) position (+ 1 counter) ))
        (else (auxGetSeccion message start (+ 1 beggining) (+ 1 end) position counter))
        ))

(define (madeNode node)
 (list (auxMadeNode node (string-length node) 0 0 1 '())))

(define (auxMadeNode node length  position auxiliarPos counter newNode)
  (cond ((equal? counter length) (append newNode (list (string->number (substring node position counter)))))
        ((equal? (substring node auxiliarPos counter) " ") (auxMadeNode node length  counter (+ 1 auxiliarPos) (+ 1 counter) (append newNode (list (substring node position (- counter 1))))) )
        (else (auxMadeNode node length  position (+ 1 auxiliarPos) (+ 1 counter) newNode) )
        ))

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

(define (messageManagement message)
   (format "~a" (findRoute (getSeccion message 1)
             (getSeccion message 2)
             (list (append (list (getSeccion message 1)) (list (getSeccion message 1)) '(0) ))
             '()
             
             (madeGraph (getSeccion message 3))))
)

;servidor
(require racket/tcp)

(define (handle-client client-input client-output  message)
  (displayln "Conexión establecida.")
  
  ; Lee el mensaje del cliente
  
  (displayln (string-append "Mensaje del cliente: " message))
  
  ; Envía una respuesta al cliente
  (displayln "Enviando respuesta al cliente...")
  (write (messageManagement message) client-output) ; Respuesta predeterminada
  
  (flush-output client-output)
  
  (displayln "Conexión cerrada.")
  
  (close-input-port client-input)
  (close-output-port client-output))

(define (start-server port)
  (define listener (tcp-listen port 5 #t))
  (displayln (string-append "Servidor escuchando en el puerto " (number->string port)))
  (let loop ()
    (define-values (client-input client-output) (tcp-accept listener))
    (thread (λ () (handle-client client-input client-output (read-line client-input))))
    (loop)))

(define (main)
  (displayln "Esperando conexiones...")
  (start-server 8080))

(main) 