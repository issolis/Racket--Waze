#lang racket
;--------------------------------conexión con el servidor--------------------------------
(require racket/tcp)
;Esta función envía un mensaje al servidor
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




