#lang racket/base

(provide start)

(require web-server/http/request-structs
         (only-in net/url
                  url->string)
         (only-in (file "respond.rkt")
                  respond))

(define (start req)
  (define method (request-method req))
  (define body (request-post-data/raw req))
  (define url/string (url->string (request-uri req)))
  (define body-length
    (cond [(bytes? body)
           (bytes-length body)]
          [else
           0]))
  (define message
    (cond [(zero? body-length)
           (format "~a ~a"
                   (string-upcase (bytes->string/utf-8 method))
                   url/string)]
          [else
           (format "~a ~a byte(s) to ~a"
                   (string-upcase (bytes->string/utf-8 method))
                   body-length
                   url/string)]))
  (displayln message)
  (respond))
