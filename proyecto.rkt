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






