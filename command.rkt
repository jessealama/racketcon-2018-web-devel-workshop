#lang racket/base

(require (only-in racket/match
                  match)
         #;raco/command-name
         (only-in web-server/servlet-env
                  serve/servlet)
         (prefix-in first:
                    (only-in (file "first.rkt")
                             start))
         (prefix-in second:
                    (only-in (file "second.rkt")
                             start))
         (prefix-in third:
                    (only-in (file "third.rkt")
                             start))
         (prefix-in fourth:
                    (only-in (file "fourth.rkt")
                             start)))

(define opt-port 31872)

(module+ raco
  (define command-name (with-handlers ([exn:fail? (λ _ #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(define (dispatch command-name)
  (match command-name
    [(or #f "help")
     (handle-help)]
    ["first"
     (handle-first)]
    ["second"
     (handle-second)]
    ["third"
     (handle-third)]
    ["fourth"
     (handle-fourth)]
    [else
     (handle-unknown command-name)]))

(define (handle-unknown command)
  (displayln (format "Unknown command `~a`." command))
  (handle-help))

(define (run-server start)
  (serve/servlet start
                 #:command-line? #f
                 #:stateless? #t
                 #:launch-browser? #f
                 #:servlet-regexp #rx".*"
                 #:port opt-port))

(define (handle-first)
  (run-server first:start))

(define (handle-second)
  (run-server second:start))

(define (handle-third)
  (run-server third:start))

(define (handle-fourth)
  (run-server fourth:start))

(define (handle-help)
  (displayln "Available commands:
help        show this message
first       launch the first server
second      launch the second server
third       launch the third server
fourth      launch the fourth server"))
