#lang racket/gui

(require racket/class)
(require racket/tcp)
(require racket/format)

(define (start message)
  (define-values (client-input client-output) (tcp-connect "127.0.0.1" 8080))
  (displayln "Conectado al servidor.")
  
  ; Envía un mensaje "hola" al servidor
  (displayln "Enviando grafo al servidor..")
  (write message client-output)
  (newline client-output) ; Añade una nueva línea para indicar el final del mensaje
  (flush-output client-output)
  
  ; Lee la respuesta del servidor
  (define response (read-line client-input))
  (displayln (string-append "Respuesta del servidor: " response))
  
  (close-input-port client-input)
  (close-output-port client-output))

(define window
  (new frame%
       [label "Graph Map Viewer"]
       [width 1000]
       [height 650]
       [style '(no-resize-border)]))

(define content-panel
  (new horizontal-panel%
       [parent window]
       [min-width 200]
       [style '(border)]))

(define node-panel
  (new vertical-panel%
       [parent content-panel]
       [style '(border)]))

(define set-city-panel
  (new vertical-panel%
       [parent node-panel]
       [style '(border)]))

(define set-street-panel
  (new vertical-panel%
       [parent node-panel]
       [style '(border)]))

(define start-finish-panel
  (new vertical-panel%
       [parent node-panel]
       [style '(border)]))

(define custom-canvas%
  (class canvas%
    (super-new)
    (define/override (on-paint)
      (let* ((canvas-width (send this get-width))
             (canvas-height (send this get-height))
             (circle-radius 30)
             (circle-x (- (/ canvas-width 2) circle-radius))
             (circle-y (- (/ canvas-height 2) circle-radius))
             (x-pos 0)
             (y-pos 0)
             (copy-node-positions (append node-positions '()))
             ;create arris variables
             (start-node 0)
             (end-node 0)
             (start-pos '())
             (end-pos '())
             (temp-line '()))
        (displayln copy-node-positions)
        (define dc (send this get-dc))
        (send dc set-pen "blue" 2 'solid) ; Set the outline color and width
        (if circle-shown?
            (begin
              (for([i (length cities-list)])
                (set! x-pos (first (first copy-node-positions)))
                (set! y-pos (first(rest (first copy-node-positions))))
                (send dc set-brush "red" 'solid) ; Set the fill color
                (send dc draw-ellipse x-pos y-pos (* 2 circle-radius) (* 2 circle-radius))
                (set! copy-node-positions (rest copy-node-positions))
                )
              ;(set! circle-shown? #f)
              ;(send dc set-brush "red" 'solid) ; Set the fill color
              ;(send dc draw-ellipse 160 229.47 (* 2 circle-radius) (* 2 circle-radius))
              ) ; Draw the red circle
            (send dc set-brush "white" 'solid) ; Set the fill color to white if circle not shown
            )
        (if arris-shown?
            (begin
              (send dc set-pen "black" 2 'solid)
              (send dc set-brush "black" 'solid)
              (for([i graph-material])
                (set! start-node (list-ref i 1))
                (set! end-node (list-ref i 0))
                (set! start-pos (list-ref node-positions (- start-node 1)))
                (set! end-pos (list-ref node-positions (- end-node 1)))
                (displayln start-pos)
                (displayln end-pos)
                (send dc draw-line (+ 30(list-ref start-pos 0))
                                      (+ 30(list-ref start-pos 1))
                                      (+ 30(list-ref end-pos 0))
                                      (+ 30(list-ref end-pos 1)))
                ; Draw a pointer at the end of the line
                        (send dc draw-line (+ 30(list-ref end-pos 0)) (+ 30(list-ref end-pos 1))
                                      (- (+ 30(list-ref end-pos 0)) (* 10 (cos (/ 0.7854 2))))
                                      (- (+ 30(list-ref end-pos 1)) (* 10 (sin (/ 0.7854 2)))))
                        (send dc draw-line (+ 30(list-ref end-pos 0)) (+ 30(list-ref end-pos 1))
                                      (- (+ 30(list-ref end-pos 0)) (* 10 (cos (/ 0.7854 2))))
                                      (+ (+ 30(list-ref end-pos 1)) (* 10 (sin (/ 0.7854 2)))))
                )
              )
            (send dc set-brush "white" 'solid)
            )))))

(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

;(begin
  ;(for([i graph-material])
    ;(display (first i))))


(define graphical-graph
  (new custom-canvas%
       [parent content-panel]
       [style '(border)]
       [min-width 600]
       [min-height 300]))

(define arris-shown? #f) ; Flag to control whether to paint the canvas
(define circle-shown? #f) ; Flag to control whether to show the nodes

(define graph-material'()) ;Global list where the graph will be stored
(define node-positions '((80 230) (190 110) (300 10) (410 110) (520 230) (410 325) (300 440) (190 325)))

;====== Set connections & weights ======
(define (check-city cities city)
  (cond ((null? cities) (displayln "Need to add a city"))
        (else (check-city-aux cities city 1))
        ))

(define (check-city-aux cities city counter)
  (cond ((null? cities) (displayln "The city is not in the list"))
        ((equal? city (send (first cities) get-value)) counter)
        (else (check-city-aux (rest cities) city (+ 1 counter)))
        ))

(define (set-arris cities from-city to-city weight)
  (let( (a (check-city cities from-city))
        (b (check-city cities to-city))
        (temp '())
        )
    (displayln a)
    (displayln b)
    (displayln weight)
    (define result-list(cons b (cons a (cons weight temp))))
    (set! graph-material (cons result-list graph-material))
    (displayln result-list)
    (displayln graph-material)
    )
  )

;====== Graphic Nodes ======


;====== Capture Nodes (Cities) ======

(define cities-list '()) ;Global list to store the cities provided by the user

(define (create-city-field)
  (define city-field(new text-field%[parent set-city-panel] [label #f]))
  (set! cities-list (cons city-field cities-list))
  city-field)

(define (get-cities-list)
  (for-each
   (lambda (city-field)
     (displayln (format "Field: ~a, Value: ~a" (send city-field get-label) (send city-field get-value))))
  (reverse cities-list)))

(define node-label
  (new message%
       [parent set-city-panel]
       [label "List of cities: "]))

(define addCityBtn
  (new button%
       [label "Add City"]
       [parent set-city-panel]
       [callback
        (lambda (button event) (create-city-field))]))

(define defineNodesBtn
  (new button%
       [label "Define cities"]
       [parent set-city-panel]
       [callback
        (lambda (button event)
          (set! circle-shown? #t)
          (send graphical-graph refresh))])) ; Trigger a canvas refresh

;====== Capture final response ======
;(define graph-arris '((3 1 5) (3 2 3) (2 1 1)))

(define (arrises-to-string nodes)
  (string-join
   (map
    (lambda (lst)
      (apply format "(~a ~a ~a)" lst))nodes)"")
  )

(define (best-path)
  (let (
         (arrises (arrises-to-string graph-material))
         (start-node (number->string(check-city (reverse cities-list) (send start-field get-value))))
         (finish-node (number->string(check-city (reverse cities-list) (send finish-field get-value))))
         (res '())
        )
    (set! res(string-append " " start-node " " finish-node " " arrises " "))
    (displayln res)
    (start res))
    )

;====== SetStreet Section ======
(define set-street-section-label
  (new message%
       [parent set-street-panel]
       [label "Set Street options: "]))
;From row
(define from-section
  (new horizontal-panel%
       [parent set-street-panel]))

(define from-label
  (new message%
       [parent from-section]
       [label "From: "]))

(define from-field
  (new text-field%
       [parent from-section] [label #f]))

;To row
(define to-section
  (new horizontal-panel%
       [parent set-street-panel]))

(define to-label
  (new message%
       [parent to-section]
       [label "To: "]))

(define to-field
  (new text-field%
       [parent to-section] [label #f]))

;Weight row
(define weight-section
  (new horizontal-panel%
       [parent set-street-panel]))

(define weight-label
  (new message%
       [parent weight-section]
       [label "Distance: "]))

(define weight-field
  (new text-field%
       [parent weight-section] [label #f]))

;Set street button
(define setStreetBtn
  (new button%
       [label "Set Street"]
       [parent set-street-panel]
       [callback
        (lambda (button event) (set-arris (reverse cities-list)
                                          (send from-field get-value)
                                          (send to-field get-value)
                                          (send weight-field get-value))
          (set! arris-shown? #t)
          (send graphical-graph refresh))]))

;====== Start-Finish Section ======
(define start-finish-section-label
  (new message%
       [parent start-finish-panel]
       [label "Start / Finish section"]))

(define start-row
  (new horizontal-panel%
       [parent start-finish-panel]))

(define start-label
  (new message%
       [parent start-row]
       [label "Start: "]))

(define start-field
  (new text-field%
       [parent start-row] [label #f]))

(define finish-row
  (new horizontal-panel%
       [parent start-finish-panel]))

(define finish-label
  (new message%
       [parent finish-row]
       [label "Finish: "]))

(define finish-field
  (new text-field%
       [parent finish-row] [label #f]))


(define defineSFBtn
  (new button%
       [label "Search best path"]
       [parent start-finish-panel]
       [callback
        (lambda (button event) (best-path))]))

;====== Create Graph ======
(define create-btn
  (new button%
       [label "Create graph"]
       [parent window]
       [callback
        (lambda (button event) (get-cities-list))]))


(send window show #t)