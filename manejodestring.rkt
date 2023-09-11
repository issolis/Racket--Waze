#lang racket
(define (getSeccion message seccion)
  (auxGetSeccion message 0 0 1 seccion 0))

(define (auxGetSeccion message start beggining end position counter)
  (cond ((equal? counter 3) (substring message start (- (string-length message) 1)))
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